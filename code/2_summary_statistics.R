# This script estimates summary statistics and miscellaneous calculations
# in Public R&D and Spillovers: Evidence from NASA Patents (Chau, 2023)

setwd('~/Dropbox/Work/Research/')

# Load packages and functions
source('NASA/replication_package/R/functions.R')


# CITATION DIFFERENCE IN MEANS, NASA VS NON NASA PATENTS -----------------------


diff_in_means_df = fread('NASA/final_output/diff_in_means_df.csv')
n_nasa = table(diff_in_means_df$nasa)
n_nasa_footnote = paste0(format(n_nasa[2], big.mark = ','), ' / ', 
                         format(n_nasa[1], big.mark = ','))

diff_in_means_1 = felm(lifetime_citations ~ nasa, data = diff_in_means_df)
diff_in_means_2 = felm(lifetime_citations ~ nasa|uspc_sub, data = diff_in_means_df)
diff_in_means_3 = felm(lifetime_citations ~ nasa|uspc_sub + year_issue, 
                       data = diff_in_means_df)
diff_in_means_list = list(diff_in_means_1, diff_in_means_2, diff_in_means_3)
stargazer(diff_in_means_list, 
          covariate.labels = c('$\\mathbb{I}$(NASA)', 'Constant'),
          dep.var.labels = 'Lifetime Citations',
          add.lines = list(c("Technology FE", "N", "Y", "Y"),
                           c("Issue Year FE", "N", "N", "Y"),
                           c('NASA/Non-NASA Obs.', 
                             n_nasa_footnote, 
                             n_nasa_footnote,
                             n_nasa_footnote)),
          notes = 'Robust s.e. in parentheses. *p<0.1; **p<0.05; ***p<0.01',
          notes.append = FALSE,
          header = FALSE,
          type = 'latex',
          title = 'Difference in Lifetime Citations, NASA \\& Non-NASA Patents',
          column.sep.width = "0pt",
          table.layout = '=dc#-t-a=n',
          label = 'tab:diff_means_table',
          table.placement = '!h',
          out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/diff_in_means_table.tex')



# BASELINE YEAR BALANCE TABLE --------------------------------------------------


event_study_df = fread('NASA/final_output/event_study_df.csv')

# Count baseline year number of classes
n_control = length(unique(event_study_df[treated == 0 & year == 1957]$uspc_sub))
n_treat = length(unique(event_study_df[treated == 1 & year == 1957]$uspc_sub))

# Subset data to only relevant variables
baseline_year_df = event_study_df[year == 1957, 
                                  .(treated, 
                                    issued, 
                                    citations_year, 
                                    citations_year_loo, 
                                    citations_lifetime,
                                    citations_lifetime_loo)]

# Estimate a balance regression for each covariate
# Note: Usually kable's digit and big.mark options make everything pretty,
# but adding an extra row with class counts without decimals + dataframe columns
# being a single type means some fixing needs to be done manually
covariate_names = c('Patents Issued',
                    'Citations',
                    'Citations (Leave-one-out)',
                    'Lifetime Citations',
                    'Lifetime Citations (LOO)')
balance_regressions = lapply(baseline_year_df %>% select(-treated), 
                             function(x) {
                               reg = felm(x ~ treated, data = baseline_year_df)
                               coefs = coef(summary(reg))
                               data.frame('control' = round(coefs[1], 3),
                                          'treat' = round(coefs[1] + coefs[2], 3),
                                          'diff' = round(coefs[2], 3),
                                          'p_value' = round(coefs[8], 3))
                             }) %>% bind_rows() %>% 
  mutate(covariate = covariate_names) %>% 
  select(covariate, everything())

# Pad zeros to match digits in table
balance_regressions$p_value = kable_zeros(balance_regressions$p_value)
balance_regressions %<>% rbind(list('Narrow USPC Count', 
                                    format(n_control, big.mark = ','), 
                                    format(n_treat, big.mark = ','), 
                                    NA, 
                                    ''))

# Make into latex format
balance_regressions %>% 
  kable_formatted(caption = 'Summary Statistics, 1957',
        label = 'balance_table',
        col.names = c('Covariate',
                      'Control Mean',
                      'Treat. Mean',
                      'Diff. in Means',
                      'p-value'),
        align = 'lcccc') %>% 
  row_spec(5, hline_after = TRUE) %>% 
  kable_styling(latex_options = 'hold_position') %>% 
  save_kable(
    '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/balance_table.tex'
  )

# Note: kable uses toprule, midrule, and bottomrule, while stargazer uses a 
# mix of hrule and [1.8ex] spacers. Because there are more regression tables
# in the paper, I manually match the kable output to the stargazer format 
# afterwards.


# TOP TREATED AND CONTROL CLASSES ----------------------------------------------


event_study_df = fread('NASA/final_output/event_study_df.csv')
uspc_names = fread('Patents/input/uspc_classes.csv')

total_patents = event_study_df %>% 
  select(uspc_sub, treated, issued, citations_lifetime) %>% 
  group_by(uspc_sub, treated) %>% 
  summarize(issued = sum(issued),
            citations_lifetime = sum(citations_lifetime),
            .groups = 'keep') %>% 
  ungroup() %>% 
  arrange(-issued)

total_patents %>% 
  filter(treated == 1) %>% 
  select(-treated) %>% 
  slice_head(n = 10) %>% 
  kable_formatted(caption = 'Treated Classes with Most Patents, 1948-1980',
                  label = 'treated_classes',
                  col.names = c('USPC Subclass',
                                'Patents',
                                'Citations'),
                  align = 'lc') %>% 
  kable_styling(latex_options = 'hold_position') %>% 
  # save_kable(
  #   '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/treated_classes.tex'
  # )
  
total_patents %>% 
  filter(treated == 0) %>% 
  select(-treated) %>% 
  slice_head(n = 10) %>% 
  kable_formatted(caption = 'Control Classes with Most Patents, 1948-1980',
                  label = 'control_classes',
                  col.names = c('USPC Subclass',
                                'Patents',
                                'Citations'),
                  align = 'lc') %>% 
  kable_styling(latex_options = 'hold_position') %>% 
  # save_kable(
  #   '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/control_classes.tex'
  # )
  
  
# MISCELLANEOUS CALCULATIONS ---------------------------------------------------


# How many classes did NASA create (hold first patent for)
nasa_seeded_classes = fread('NASA/final_output/first_patent_each_class.csv')
table(nasa_seeded_classes$nasa)

# How many were seeded from 1958 to 1974
table(nasa_seeded_classes[year(disp_dt_parsed) %in% 1958:1974]$nasa)




