# This script takes the output data from 1_prepare_data.R to estimate the main
# analyses in Public R&D and Spillovers: Evidence from NASA Patents (Chau, 2022)

setwd('~/Dropbox/Work/Research')
figures_folder = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/figures/baseline_event_studies/'
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
                'citations_20_yr_loo_b_excl_nasa',
                'citations_lifetime_uspc_count',
                'citations_lifetime_loo_uspc_share',
                'citations_lifetime_loo_b_uspc_share',
                'citations_year_uspc_count',
                'citations_year_loo_uspc_share',
                'citations_year_loo_b_uspc_share',
                'citations_mean_lag',
                'citations_max_lag',
                'generality_lifetime_hhi',
                'generality_year_hhi',
                'breakthrough_90',
                'breakthrough_95',
                'breakthrough_99') 

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


# Overlaid plots (for slides)
# Extract coefficients from both models
model_1 = baseline_event_studies[[5]]
model_2 = baseline_event_studies[[7]]
model_3 = baseline_event_studies[[9]]

model_1_name = 'Baseline'
model_2_name = 'Outside Narrow Class'
model_3_name = 'Outside Broad Class'

x_range = 1948:1980
treat_time = 1958
x_lab = 'Year'
y_lab = 'Estimate'
point_size = 3
bar_size = 0.6

coef_1 = plot_event_study_prep(model_1)
coef_2 = plot_event_study_prep(model_2)
coef_3 = plot_event_study_prep(model_3)

# Stack into one long dataset
coef = bind_rows(coef_1, coef_2, coef_3, .id = 'model')

coef[, x := rep(x_range, 3)]
coef[, x := case_when(model == '1' ~ x - 0.15, 
                      model == '3' ~ x + 0.15,
                      TRUE ~ x)]

coef[, model := factor(case_when(model == '1' ~ model_1_name, 
                                 model == '2' ~ model_2_name,
                                 TRUE ~ model_3_name),
                       levels = c(model_1_name, model_2_name, model_3_name))]

citations_year_overlaid = coef %>% 
  ggplot(aes(x = x, y = Estimate, color = model, shape = model)) +
  geom_vline(xintercept = (treat_time - 0.5), alpha = 0.5, linetype = 'dashed') + 
  geom_hline(yintercept = 0, size = 0.5, linetype = 'dashed') +
  geom_errorbar(aes(ymin = low, ymax = high), 
                size = bar_size, 
                alpha = 0.5,
                width = 0.6) +
  geom_linerange(aes(ymin = low_sup_t, ymax = high_sup_t), 
                 size = bar_size,
                 alpha = 0.5) +
  geom_point(size = point_size,
             alpha = 0.8) +
  geom_line(alpha = 0.8) +
  scale_x_continuous(breaks = x_range[c(TRUE, FALSE)]) +
  ggplot_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab(x_lab) +
  ylab(y_lab) +
  scale_color_manual(values = c('honeydew4', 'aquamarine4', 'darkorange'),
                     name = 'Model') +
  scale_shape_manual(values = c(15, 17, 19),
                     name = 'Model')

ggsave(filename = paste0(figures_folder, 'citations_year_loo_overlaid_did.pdf'), 
       plot = citations_year_overlaid,
       width = plot_width + 2,
       height = plot_height)       


model_1 = baseline_event_studies[[11]]
model_2 = baseline_event_studies[[13]]
model_3 = baseline_event_studies[[15]]

model_1_name = 'Baseline'
model_2_name = 'Outside Narrow Class'
model_3_name = 'Outside Broad Class'

x_range = 1948:1980
treat_time = 1958
x_lab = 'Year'
y_lab = 'Estimate'
point_size = 3
bar_size = 0.6

coef_1 = plot_event_study_prep(model_1)
coef_2 = plot_event_study_prep(model_2)
coef_3 = plot_event_study_prep(model_3)

# Stack into one long dataset
coef = bind_rows(coef_1, coef_2, coef_3, .id = 'model')

coef[, x := rep(x_range, 3)]
coef[, x := case_when(model == '1' ~ x - 0.15, 
                      model == '3' ~ x + 0.15,
                      TRUE ~ x)]

coef[, model := factor(case_when(model == '1' ~ model_1_name, 
                                 model == '2' ~ model_2_name,
                                 TRUE ~ model_3_name),
                       levels = c(model_1_name, model_2_name, model_3_name))]

citations_lifetime_overlaid = coef %>% 
  ggplot(aes(x = x, y = Estimate, color = model, shape = model)) +
  geom_vline(xintercept = (treat_time - 0.5), alpha = 0.5, linetype = 'dashed') + 
  geom_hline(yintercept = 0, size = 0.5, linetype = 'dashed') +
  geom_errorbar(aes(ymin = low, ymax = high), 
                size = bar_size, 
                alpha = 0.5,
                width = 0.6) +
  geom_linerange(aes(ymin = low_sup_t, ymax = high_sup_t), 
                 size = bar_size,
                 alpha = 0.5) +
  geom_point(size = point_size,
             alpha = 0.8) +
  geom_line(alpha = 0.8) +
  scale_x_continuous(breaks = x_range[c(TRUE, FALSE)]) +
  ggplot_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab(x_lab) +
  ylab(y_lab) +
  scale_color_manual(values = c('honeydew4', 'aquamarine4', 'darkorange'),
                     name = 'Model') +
  scale_shape_manual(values = c(15, 17, 19),
                     name = 'Model')

ggsave(filename = paste0(figures_folder, 'citations_lifetime_loo_overlaid_did.pdf'), 
       plot = citations_lifetime_overlaid,
       width = plot_width + 2,
       height = plot_height)       

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


# Citation USPC counts (appendix)
stargazer(baseline_did[c(23, 20)],
          covariate.labels = c('$\\mathbb{I}$(NASA)'),
          dep.var.labels = c('No. Citing Classes, Yearly', 
                             'No. Citing Classes, Lifetime'),
          add.lines = list(c('Subclass FE', 'Y', 'Y'),
                           c('Year FE', 'Y', 'Y')),
          notes = c('Subclass clustered s.e. *p<0.1; **p<0.05; ***p<0.01'),
          notes.append = FALSE,
          header = FALSE,
          table.layout = '=dc#-t-as=n',
          keep.stat = 'n',
          type = 'latex',
          title = 'Difference in Differences Estimates, Number of Citing Classes',
          label = 'tab:static_did_uspc_count',
          column.sep.width = "0pt",
          table.placement = '!h',
          out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/baseline_did/citations_uspc_count_table.tex')


# Citation HHI (appendix)
stargazer(baseline_did[c(29, 28)],
          covariate.labels = c('$\\mathbb{I}$(NASA)'),
          dep.var.labels = c('HHI, Yearly', 
                             'HHI, Lifetime'),
          add.lines = list(c('Subclass FE', 'Y', 'Y'),
                           c('Year FE', 'Y', 'Y')),
          notes = c('Subclass clustered s.e. *p<0.1; **p<0.05; ***p<0.01'),
          notes.append = FALSE,
          header = FALSE,
          table.layout = '=dc#-t-as=n',
          keep.stat = 'n',
          type = 'latex',
          title = 'Difference in Differences Estimates, Citation HHI',
          label = 'tab:static_did_hhi',
          column.sep.width = "0pt",
          table.placement = '!h',
          out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/baseline_did/citations_hhi_table.tex')


# Citation lag (appendix)
stargazer(baseline_did[c(26, 27)],
          covariate.labels = c('$\\mathbb{I}$(NASA)'),
          dep.var.labels = c('Mean Citation Lag', 'Maximum Citation Lag'),
          add.lines = list(c('Subclass FE', 'Y', 'Y'),
                           c('Year FE', 'Y', 'Y')),
          notes = c('Subclass clustered s.e. *p<0.1; **p<0.05; ***p<0.01'),
          notes.append = FALSE,
          header = FALSE,
          table.layout = '=dc#-t-as=n',
          keep.stat = 'n',
          type = 'latex',
          title = 'Difference in Differences Estimates, Mean and Maximum Citation Lag',
          label = 'tab:static_did_citation_lag',
          column.sep.width = "0pt",
          table.placement = '!h',
          out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/baseline_did/citations_lag_table.tex')


# Breakthrough (appendix)
stargazer(baseline_did[c(30, 31, 32)],
          covariate.labels = c('$\\mathbb{I}$(NASA)'),
          dep.var.labels = c('90th Percentile', 
                             '95th Percentile',
                             '99th Percentile'),
          add.lines = list(c('Subclass FE', 'Y', 'Y', 'Y'),
                           c('Year FE', 'Y', 'Y', 'Y')),
          notes = c('Subclass clustered s.e. *p<0.1; **p<0.05; ***p<0.01'),
          notes.append = FALSE,
          header = FALSE,
          table.layout = '=dc#-t-as=n',
          keep.stat = 'n',
          type = 'latex',
          title = 'Difference in Differences Estimates, Blockbuster Patents',
          label = 'tab:static_did_breakthroughs',
          column.sep.width = "0pt",
          table.placement = '!h',
          out = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/tables/baseline_did/citations_breakthroughs_table.tex')


# DROP VERMIN STUFF ------------------------------------------------------------

vermin = event_study_df[str_detect(uspc_sub, '43_', negate = TRUE)]

# Estimate regressions
vermin_event_studies = mclapply(X = outcomes, 
                                  mc.cores = core_count,
                                  FUN = function(y){
                                    event_study_formula = as.formula(paste0(y, rhs))
                                    felm(event_study_formula, data = vermin)  
                                  })
names(vermin_event_studies) = outcomes

# Add sup-t confidence bands to all regressions
vermin_event_studies = mclapply(X = vermin_event_studies, 
                                  mc.cores = core_count,
                                  FUN = function(felm_object){
                                    felm_object$sup_t = calculate_sup_t(felm_object)
                                    return(felm_object)
                                  })

# Plot and save event studies
vermin_event_study_plots = mclapply(X = seq_along(vermin_event_studies),
                             mc.cores = core_count,
                             FUN = function(i){
                               felm_object = vermin_event_studies[[i]]
                               outcome = names(vermin_event_studies)[i]
                               
                               plot_event_study(model = felm_object,
                                                treat_time = 1958,
                                                x_lab = 'Year', 
                                                y_lab = 'Estimate', 
                                                x_range = 1948:1980, 
                                                point_size = 3, 
                                                bar_size = 0.6) %>%   
                                 ggsave(filename = paste0('/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/figures/vermin/', 
                                                          outcome, '_did.pdf'), 
                                        plot = .,
                                        width = plot_width,
                                        height = plot_height)                               
                             })
