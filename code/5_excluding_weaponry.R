# This script takes the output data from 1_prepare_data.R to estimate the main
# analyses in Public R&D and Spillovers: Evidence from NASA Patents (Chau, 2022)

setwd('~/Dropbox/Work/Research')
figures_folder = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/figures/excluding_weaponry/'
source('NASA/replication_package/R/functions.R')

# Set number of cores for parallelization
core_count = detectCores() - 2


# EVENT STUDY ------------------------------------------------------------------


event_study_df = fread('NASA/final_output/event_study_df.csv')

event_study_df[, D := as.factor(D)]
event_study_df[, D := relevel(D, ref = '-1')]

# Omit military classes
military_classes = '89\\_|42\\_|86\\_|102\\_|124\\_|149\\_'
event_study_df = event_study_df[, military_class := ifelse(
  str_detect(uspc_sub, military_classes), 1, 0
)]
event_study_df = event_study_df[military_class == 0]


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
no_dod_event_studies = mclapply(X = outcomes, 
                                  mc.cores = core_count,
                                  FUN = function(y){
                                    event_study_formula = as.formula(paste0(y, rhs))
                                    felm(event_study_formula, data = event_study_df)  
                                  })
names(no_dod_event_studies) = outcomes

# Add sup-t confidence bands to all regressions
no_dod_event_studies = mclapply(X = no_dod_event_studies, 
                                  mc.cores = core_count,
                                  FUN = function(felm_object){
                                    felm_object$sup_t = calculate_sup_t(felm_object)
                                    return(felm_object)
                                  })

# Plot and save event studies
event_study_plots = mclapply(X = seq_along(no_dod_event_studies),
                             mc.cores = core_count,
                             FUN = function(i){
                               felm_object = no_dod_event_studies[[i]]
                               outcome = names(no_dod_event_studies)[i]
                               
                               plot_event_study(model = felm_object,
                                                treat_time = 1958,
                                                x_lab = 'Year', 
                                                y_lab = 'Estimate', 
                                                x_range = 1948:1980, 
                                                point_size = 3, 
                                                bar_size = 0.6) %>%   
                                 ggsave(filename = paste0(figures_folder, outcome, '_no_dod.pdf'), 
                                        plot = .,
                                        width = plot_width,
                                        height = plot_height)                               
                             })

