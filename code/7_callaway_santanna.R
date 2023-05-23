setwd('~/Dropbox/Work/Research')
figures_folder = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/figures/callaway_santanna/'
source('NASA/replication_package/R/functions.R')

library(did)

# Set number of cores for parallelization
core_count = detectCores() - 2

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
                'generality_year_hhi') 

event_study_df[, g_var := ifelse(treated == 1, 1958, 0)]
uspc_id_map = unique(event_study_df$uspc_sub) %>% 
  data.table('uspc_sub' = .) %>% 
  rownames_to_column(var = 'id_var') %>% 
  mutate(id_var = as.numeric(id_var))
  
event_study_df = merge(event_study_df, uspc_id_map, by = 'uspc_sub', all.x = TRUE)

plot_callaway_santanna = function(outcome, time = 'year', id = 'id_var',
                                  cohort = 'g_var', df = event_study_df,
                                  parallelize = FALSE, n_cores = 1,
                                  x_lab = 'Event Time', y_lab = 'Estimate',
                                  point_size = 3, bar_size = 0.5,
                                  plot_width = 7, plot_height = 4) {
  # Estimate
  group_time_att = att_gt(yname = outcome,
                          tname = time,
                          idname = id,
                          gname = cohort,
                          data = df,
                          allow_unbalanced_panel = TRUE,
                          pl = parallelize,
                          cores = n_cores)
  aggregated_att = aggte(group_time_att, type = 'dynamic')
  
  # Plot and save
  cs_plot = ggdid(aggregated_att) +
    ggplot_theme() +
    theme(legend.position = 'none') +
    geom_vline(xintercept = -0.5, alpha = 0.5, linetype = 'dashed') +
    geom_errorbar(color = 'gray', size = bar_size) +
    geom_point(color = 'darkorange', size = point_size) +
    scale_color_manual(values = c('gray', 'gray')) + 
    labs(title = '',
         x = x_lab,
         y = y_lab)
    ggsave(filename = paste0(figures_folder, outcome, '_cs.pdf'),
          plot = cs_plot,
          width = plot_width,
          height = plot_height)
}

# The CS function isn't run in parallel because the outer loop is
cs_plots = mclapply(outcomes, mc.cores = core_count, 
                    FUN = function(y){plot_callaway_santanna(outcome = y)})
