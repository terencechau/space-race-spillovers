# This script takes the output data from 1_prepare_data.R to estimate the main
# analyses in Public R&D and Spillovers: Evidence from NASA Patents (Chau, 2022)

setwd('~/Dropbox/Work/Research')
figures_folder = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/figures/alternative_control_grips/'
source('NASA/replication_package/R/functions.R')

# Set number of cores for parallelization
core_count = detectCores() - 2


# BASELINE EVENT STUDY ---------------------------------------------------------


event_study_df = fread('NASA/final_output/event_study_df.csv')

event_study_df[, D := as.factor(D)]
event_study_df[, D := relevel(D, ref = '-1')]

outcomes = list('issued', 
                'issued_excl_nasa',
                'applied', 
                'citations_year',
                'citations_year_excl_nasa', 
                'citations_year_loo',
                'citations_year_loo_excl_nasa', 
                'citations_year_loo_b',
                'citations_year_loo_b_excl_nasa', 
                'citations_lifetime',
                'citations_lifetime_excl_nasa', 
                'citations_lifetime_loo',
                'citations_lifetime_loo_excl_nasa', 
                'citations_lifetime_loo_b',
                'citations_lifetime_loo_b_excl_nasa', 
                'citations_20_yr',
                'citations_20_yr_excl_nasa',
                'citations_20_yr_loo_excl_nasa',
                'citations_20_yr_loo_b_excl_nasa') 

rhs = '~ D|uspc_sub + year|0|uspc_sub'

# Estimate regressions
baseline_event_studies = mclapply(X = outcomes, 
                                  mc.cores = core_count,
                                  FUN = function(y){
  event_study_formula = as.formula(paste0(y, rhs))
  felm(event_study_formula, data = event_study_df)  
})
names(baseline_event_studies) = outcomes

# Add sup-t confidence bands to all regressions
baseline_event_studies = mclapply(X = baseline_event_studies, 
                                  mc.cores = core_count,
                                  FUN = function(felm_object){
  felm_object$sup_t = calculate_sup_t(felm_object)
  return(felm_object)
})

# Plot and save event studies
event_study_plots = mclapply(X = seq_along(baseline_event_studies),
                             mc.cores = core_count,
                             FUN = function(i){
  felm_object = baseline_event_studies[[i]]
  outcome = names(baseline_event_studies)[i]
  
  plot_event_study(model = felm_object,
                   treat_time = 1958,
                   x_lab = 'Year', 
                   y_lab = 'Estimate', 
                   x_range = 1948:1980, 
                   point_size = 3, 
                   bar_size = 0.6) %>%   
  ggsave(filename = paste0(figures_folder, outcome, '_did.pdf'), 
         plot = .,
         width = plot_width,
         height = plot_height)                               
})


# PRE-POST DIFFERENCE IN DIFFERENCES REGRESSIONS -------------------------------


event_study_df[, D_indicator := ifelse(treated == 1 & year >= 1958, 1, 0)]
rhs_indicator = '~ D_indicator|uspc_sub + year|0|uspc_sub'

# Estimate regressions
baseline_did = mclapply(X = outcomes, 
                        mc.cores = core_count,
                        FUN = function(y){
                          did_formula = as.formula(paste0(y, rhs_indicator))
                          felm(did_formula, data = event_study_df)  
})
names(baseline_did) = outcomes

# Save tables

# Patent issues
stargazer(baseline_did[c(1, 2)],
          covariate.labels = c('$\\mathbb{I}$(NASA)'),
          dep.var.labels = c('Patent Issues', 'Patent Issues (Excl. NASA)'),
          add.lines = list(c('Subclass FE', 'Y', 'Y'),
                           c('Year FE', 'Y', 'Y')),
          notes = 'Subclass clustered s.e. *p<0.1; **p<0.05; ***p<0.01',
          notes.append = FALSE,
          header = FALSE,
          table.layout = '=dc#-t-as=n',
          keep.stat = 'n',
          type = 'latex',
          title = 'Difference in Differences Estimates, Patent Counts',
          label = 'tab:static_did_issue',
          column.sep.width = "0pt",
          table.placement = '!h',
          out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/baseline_did/issued_table.tex')


# Patent citations
stargazer(baseline_did[c(4, 5, 10, 11)],
          covariate.labels = c('$\\mathbb{I}$(NASA)'),
          dep.var.labels = c('Yearly', 'Yearly (Excl. NASA)', 
                             'Lifetime', 'Lifetime (Excl. NASA)'),
          add.lines = list(c('Subclass FE', 'Y', 'Y', 'Y', 'Y'),
                           c('Year FE', 'Y', 'Y', 'Y', 'Y')),
          notes = 'Subclass clustered s.e. *p<0.1; **p<0.05; ***p<0.01',
          notes.append = FALSE,
          header = FALSE,
          table.layout = '=dc#-t-as=n',
          keep.stat = 'n',
          type = 'latex',
          title = 'Difference in Differences Estimates, Patent Citations',
          label = 'tab:static_did_citations',
          column.sep.width = "0pt",
          table.placement = '!h',
          out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/baseline_did/citations_table.tex')


# Leave one out citations
# Note: the multiline comment gets messed up by stargazer, so I fixed it manually
# and commented out the save
stargazer(baseline_did[c(7, 9, 13, 15)],
          covariate.labels = c('$\\mathbb{I}$(NASA)'),
          dep.var.labels = c('Yearly', 'Yearly, Broad', 
                             'Lifetime', 'Lifetime, Broad'),
          add.lines = list(c('Subclass FE', 'Y', 'Y', 'Y', 'Y'),
                           c('Year FE', 'Y', 'Y', 'Y', 'Y')),
          notes = c('Subclass clustered s.e. *p<0.1; **p<0.05; ***p<0.01',
                    'Columns (1) and (3) exclude within-narrow subclass citations, while (2) and (4) exclude within-broad class citations. All columns exclude NASA to NASA citations.'),
          notes.append = FALSE,
          header = FALSE,
          table.layout = '=dc#-t-as=n',
          keep.stat = 'n',
          type = 'latex',
          title = 'Difference in Differences Estimates, Leave-One-Out Citations',
          label = 'tab:static_did_citations_loo',
          column.sep.width = "0pt",
          table.placement = '!h')
          # out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/baseline_did/citations_loo_table.tex')

# Fixed window citations (appendix)
stargazer(baseline_did[c(16, 17, 18, 19)],
          covariate.labels = c('$\\mathbb{I}$(NASA)'),
          dep.var.labels = c('Citations', 'Excl. NASA', 
                             'Leave-One-Out, Narrow', 'LOO, Broad'),
          add.lines = list(c('Subclass FE', 'Y', 'Y', 'Y', 'Y'),
                           c('Year FE', 'Y', 'Y', 'Y', 'Y')),
          notes = c('Subclass clustered s.e. *p<0.1; **p<0.05; ***p<0.01'),
          notes.append = FALSE,
          header = FALSE,
          table.layout = '=dc#-t-as=n',
          keep.stat = 'n',
          type = 'latex',
          title = 'Difference in Differences Estimates, 20-Year Window Citations',
          label = 'tab:static_did_20_yr',
          column.sep.width = "0pt",
          table.placement = '!h',
          out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/baseline_did/citations_20_yr_table.tex')


