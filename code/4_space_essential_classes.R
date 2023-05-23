# This script takes the output data from 1_prepare_data.R to estimate the 
# regressions using space essential classes in Public R&D and Spillovers: 
# Evidence from NASA Patents (Chau, 2022)

setwd('~/Dropbox/Work/Research')
figures_folder = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/figures/space_essential_classes/'
source('NASA/replication_package/R/functions.R')

# Set number of cores for parallelization
core_count = detectCores() - 2


# EVENT STUDY ------------------------------------------------------------------


event_study_df = fread('NASA/final_output/event_study_df.csv')

# Define space essential broad classes
related_classes = '244\\_|136\\_|149\\_|250\\_|310\\_|318\\_|323\\_|324\\_|327\\_|341\\_|342\\_|343\\_|356\\_|359\\_|374\\_|375\\_|420\\_|427\\_|438\\_|455\\_|60\\_|600\\_|702\\_|703\\_|708\\_|709\\_|710\\_|711\\_|712\\_|713\\_|714\\_|73\\_|89\\_|91\\_|102\\_374|102\\_375|102\\_376|102\\_377|102\\_378|102\\_379|102\\_380|102\\_381|2\\_2|128\\_20'     
event_study_df = event_study_df[, space_essential_class := ifelse(
  str_detect(uspc_sub, related_classes), 1, 0
)]
event_study_df[, Z := factor(space_essential_class * event_time)]
event_study_df[, Z := relevel(Z, ref = '-1')]

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

rhs = '~ Z|uspc_sub + year|0|uspc_sub'

# Estimate regressions
essential_event_studies = mclapply(X = outcomes, 
                                  mc.cores = core_count,
                                  FUN = function(y){
                                    event_study_formula = as.formula(paste0(y, rhs))
                                    felm(event_study_formula, data = event_study_df)  
                                  })
names(essential_event_studies) = outcomes

# Add sup-t confidence bands to all regressions
essential_event_studies = mclapply(X = essential_event_studies, 
                                  mc.cores = core_count,
                                  FUN = function(felm_object){
                                    felm_object$sup_t = calculate_sup_t(felm_object)
                                    return(felm_object)
                                  })

# Plot and save event studies
event_study_plots = mclapply(X = seq_along(essential_event_studies),
                             mc.cores = core_count,
                             FUN = function(i){
                               felm_object = essential_event_studies[[i]]
                               outcome = names(essential_event_studies)[i]
                               
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


# Overlaying baseline and instrumented plots
rhs = '~ D|uspc_sub + year|0|uspc_sub'

event_study_df[, D := as.factor(D)]
event_study_df[, D := relevel(D, ref = '-1')]

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

overlaid_event_study_plots = mclapply(X = seq_along(essential_event_studies),
                             mc.cores = core_count,
                             FUN = function(i){
                               felm_object_1 = baseline_event_studies[[i]]
                               felm_object_2 = essential_event_studies[[i]]
                               outcome = names(essential_event_studies)[i]
                               
                               plot_event_study_overlaid(model_1 = felm_object_1,
                                                         model_2 = felm_object_2,
                                                         model_1_name = 'Baseline',
                                                         model_2_name = 'Essential',
                                                         treat_time = 1958,
                                                         x_lab = 'Year', 
                                                         y_lab = 'Estimate', 
                                                         x_range = 1948:1980, 
                                                         point_size = 3, 
                                                         bar_size = 0.6) %>%   
                                 ggsave(filename = paste0(figures_folder, outcome, '_overlaid_did.pdf'), 
                                        plot = .,
                                        width = plot_width,
                                        height = plot_height)                               
                             })

# PRE-POST DIFFERENCE IN DIFFERENCES REGRESSIONS -------------------------------


event_study_df[, Z_indicator := ifelse(
  space_essential_class == 1 & year >= 1958, 1, 0
)]
rhs_indicator = '~ Z_indicator|uspc_sub + year|0|uspc_sub'

# Estimate regressions
essential_did = mclapply(X = outcomes, 
                        mc.cores = core_count,
                        FUN = function(y){
                          did_formula = as.formula(paste0(y, rhs_indicator))
                          felm(did_formula, data = event_study_df)  
                        })
names(essential_did) = outcomes


# SIMPLE SHARES ----------------------------------------------------------------


# For each year, out of the things NASA worked on, what's the share belonging 
# to essential classes?
nasa_panel_df = panel_df[nasa > 0]
nasa_panel_df = nasa_panel_df[, .(share_essential = sum(space_essential_class)/.N,
                                  sum_essential = sum(space_essential_class),
                                  total_classes = .N,
                                  total_patents = sum(nasa)), 
                              by = year]

year_by_year = nasa_panel_df %>% 
  ggplot(aes(x = year, y = share_essential)) +
  geom_point(color = 'aquamarine4', size = 3) +
  geom_line(color = 'aquamarine4') +
  ggplot_theme() +
  labs(x = 'Year',
       y = 'Share in Essential') +
  scale_x_continuous(breaks = seq(1960, 2014, by = 5))


ggsave(filename = paste0(figures_folder, 'year_by_year.pdf'), 
       plot = year_by_year,
       width = plot_width,
       height = plot_height)