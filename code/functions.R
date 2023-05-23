# Auxiliary functions for estimating event study regressions and other analyses
# in Public R&D and Spillovers: Evidence from NASA Patents (Chau, 2022)

library(tidyverse)
library(magrittr)
library(lubridate)
library(data.table)
library(readxl)
library(lfe)
library(suptCriticalValue)
library(knitr)
library(kableExtra)
library(stargazer)
library(parallel)
library(ggpubr)
library(readxl)

table_digits = 3
options(knitr.kable.NA = '')

plot_width = 7
plot_height = 4

kable_formatted = function(...) {
  kable(digits = table_digits, 
        format.args = list(big.mark = ','), 
        format = 'latex',
        booktabs = TRUE,
        linesep = '',
        ...)
}

kable_zeros = function(x, digits = table_digits) {
  format_string = paste0("%.0", table_digits, "f")
  ifelse(round(x, table_digits) == 0, sprintf(format_string, 0), x)
}

ceiling_decimal = function(x, level = 1) {round(x + 5*10^(-level - 1), level)}

calculate_sup_t = function(model, n_draws = 10000, confidence_level = 0.95, 
                           seed = 60637) {
  # Calculate sup-t confidence bands for event study coefficients
  # Since this is an event study, uses clustered variance-covariance matrix
  
  # Inputs: 
  # model: lfe::felm regression object
  # n_draws: number of bootstrap draws to take
  # confidence_level: confidence level
  
  # Output:
  # Dataframe with each coefficient's sup-t lower and upper bound
  
  beta = model$beta
  vcov = model$clustervcv
  std_error = sqrt(diag(vcov))
  sup_t_critical_value = suptCriticalValue(vcov_matrix = vcov,
                                           num_sim = n_draws,
                                           conf_level = confidence_level,
                                           seed = seed)
  sup_t_lb = beta - sup_t_critical_value * std_error
  sup_t_ub = beta + sup_t_critical_value * std_error
  
  df = data.frame(sup_t_lb, sup_t_ub) 
  colnames(df) = c('sup_t_lb', 'sup_t_ub')
  
  return(df)
}


ggplot_theme = function() {
  theme_light() +
    theme(plot.background = element_rect(fill = 'transparent', color = NA),
          panel.background = element_rect(fill = 'transparent', color = NA),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))
}


# plot_event_study = function(model, treat_time, x_range = NULL, 
#                             n_pre = NULL, n_post = NULL,
#                             x_lab, y_lab, point_size = 3, bar_size = 0.8){
#   # Plot event study coefficients using ggplot2
#   
#   # Inputs:
#   # model: lfe::felm regression object. Must have output from calculate_sup_t
#   # merged in
#   # treat_time: treatment time period
#   # x_range: range of time periods for estimates
#   # n_pre, n_post: number of pre and post-treatment time periods if x_range isn't provided
#   # title, x_lab, y_lab: ggplot title, x and y-axis labels
#   # point_size = coefficient point size
#   # bar_size = confidence interval and confidence band line thickness
#   
#   # Output: ggplot2 object
#   
#   # Extract coefficients
#   coef = data.frame(summary(model)$coefficients) %>% 
#     rownames_to_column(var = 'treatment_event') 
#   coef = data.table(coef)
#   
#   # In case a reduced form regression is being run, rename Z to D
#   coef[, treatment_event := str_replace(treatment_event, 'Z', 'D')]
#   
#   # Infer where the -1 relative time period is
#   minus_one = data.frame(`treatment_event` = 'D-1', `Estimate` = 0, 
#                          `Cluster.s.e.` = 0, `t.value` = 0,
#                          `Pr...t..` = 0)
#   
#   coef_break_point = (which(coef$treatment_event == 'D0') - 1)
#   
#   coef_1 = coef[1:coef_break_point, ]
#   coef_2 = coef[(coef_break_point + 1):nrow(coef), ]
#   coef = bind_rows(coef_1, minus_one, coef_2)
#   
#   coef$low = coef$Estimate - qnorm(1 - 0.05/2) * coef$Cluster.s.e.
#   coef$high = coef$Estimate + qnorm(1 - 0.05/2) * coef$Cluster.s.e.
#   
#   sup_t = model$sup_t
#   sup_t_1 = sup_t[1:coef_break_point, ]
#   sup_t_2 = sup_t[(coef_break_point + 1):nrow(coef), ]
#   sup_t = bind_rows(sup_t_1, c('sup_t_lb' = 0, 'sup_t_ub' = 0), sup_t_2) %>% 
#     drop_na()
#   
#   coef$low_sup_t = sup_t$sup_t_lb
#   coef$high_sup_t = sup_t$sup_t_ub
#   
#   if (is.null(x_range)) {
#     x_range = c(-n_pre:-2, 0:n_post)
#   }
#   
#   coef %>% 
#     ggplot(aes(x = x_range, y = Estimate)) +
#     geom_vline(xintercept = (treat_time - 0.5), alpha = 0.5, linetype = 'dashed') + 
#     geom_hline(yintercept = 0, size = 0.5, linetype = 'dashed') +
#     geom_errorbar(aes(ymin = low, ymax = high), color = 'gray', size = bar_size) +
#     geom_linerange(aes(ymin = low_sup_t, ymax = high_sup_t), color = 'gray', size = bar_size) +
#     geom_point(color = 'darkorange', size = point_size) +
#     scale_x_continuous(breaks = x_range[c(TRUE, FALSE)]) +
#     ggplot_theme() +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#     xlab(x_lab) +
#     ylab(y_lab) 
# }

# Prep Plot Data
plot_event_study_prep = function(model){
  # Inputs:
  # model: lfe::felm regression object. Must have output from calculate_sup_t
  # merged in
  
  # Output: data frame with all necessary parts for plotting
  
  # Extract coefficients
  coef = data.frame(summary(model)$coefficients) %>% 
    rownames_to_column(var = 'treatment_event') 
  coef = data.table(coef)
  
  # In case a reduced form regression is being run, rename Z to D
  coef[, treatment_event := str_replace(treatment_event, 'Z', 'D')]
  
  # Infer where the -1 relative time period is
  minus_one = data.frame(`treatment_event` = 'D-1', `Estimate` = 0, 
                         `Cluster.s.e.` = 0, `t.value` = 0,
                         `Pr...t..` = 0)
  
  coef_break_point = (which(coef$treatment_event == 'D0') - 1)
  
  coef_1 = coef[1:coef_break_point, ]
  coef_2 = coef[(coef_break_point + 1):nrow(coef), ]
  coef = bind_rows(coef_1, minus_one, coef_2)
  
  coef$low = coef$Estimate - qnorm(1 - 0.05/2) * coef$Cluster.s.e.
  coef$high = coef$Estimate + qnorm(1 - 0.05/2) * coef$Cluster.s.e.
  
  sup_t = model$sup_t
  sup_t_1 = sup_t[1:coef_break_point, ]
  sup_t_2 = sup_t[(coef_break_point + 1):nrow(coef), ]
  sup_t = bind_rows(sup_t_1, c('sup_t_lb' = 0, 'sup_t_ub' = 0), sup_t_2) %>% 
    drop_na()
  
  coef$low_sup_t = sup_t$sup_t_lb
  coef$high_sup_t = sup_t$sup_t_ub
  
  return(coef)
}

# Plot event study from felm objects
plot_event_study = function(model, treat_time, x_range = NULL, 
                            n_pre = NULL, n_post = NULL,
                            x_lab, y_lab, point_size = 3, bar_size = 0.8){
  # Plot event study coefficients using ggplot2
  
  # Inputs:
  # model: lfe::felm regression object. Must have output from calculate_sup_t
  # merged in
  # treat_time: treatment time period
  # x_range: range of time periods for estimates
  # n_pre, n_post: number of pre and post-treatment time periods if x_range isn't provided
  # title, x_lab, y_lab: ggplot title, x and y-axis labels
  # point_size = coefficient point size
  # bar_size = confidence interval and confidence band line thickness
  
  # Output: ggplot2 object
  
  # Extract coefficients
  coef = plot_event_study_prep(model)
  
  if (is.null(x_range)) {
    x_range = c(-n_pre:-2, 0:n_post)
  }
  
  coef %>% 
    ggplot(aes(x = x_range, y = Estimate)) +
    geom_vline(xintercept = (treat_time - 0.5), alpha = 0.5, linetype = 'dashed') + 
    geom_hline(yintercept = 0, size = 0.5, linetype = 'dashed') +
    geom_errorbar(aes(ymin = low, ymax = high), color = 'gray', size = bar_size) +
    geom_linerange(aes(ymin = low_sup_t, ymax = high_sup_t), color = 'gray', size = bar_size) +
    geom_point(color = 'darkorange', size = point_size) +
    scale_x_continuous(breaks = x_range[c(TRUE, FALSE)]) +
    ggplot_theme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    xlab(x_lab) +
    ylab(y_lab) 
}


# Plot overlaid event study from two felm objects
plot_event_study_overlaid = function(model_1, model_2, 
                            model_1_name,
                            model_2_name,
                            treat_time, x_range = NULL, 
                            n_pre = NULL, n_post = NULL,
                            x_lab, y_lab, point_size = 3, bar_size = 0.8){
  # Plot event study coefficients using ggplot2
  
  # Inputs:
  # model_1: lfe::felm regression object. Must have output from calculate_sup_t
  # merged in
  # model_2: lfe::felm regression object. Must have output from calculate_sup_t
  # merged in
  # treat_time: treatment time period
  # x_range: range of time periods for estimates
  # n_pre, n_post: number of pre and post-treatment time periods if x_range isn't provided
  # title, x_lab, y_lab: ggplot title, x and y-axis labels
  # point_size = coefficient point size
  # bar_size = confidence interval and confidence band line thickness
  
  # Output: ggplot2 object
  
  # Extract coefficients from both models
  coef_1 = plot_event_study_prep(model_1)
  coef_2 = plot_event_study_prep(model_2)
  
  # Stack into one long dataset
  coef = bind_rows(coef_1, coef_2, .id = 'model')
  coef[, model := ifelse(model == '1', model_1_name, model_2_name)]
  
  if (is.null(x_range)) {
    x_range = c(-n_pre:-2, 0:n_post)
  }
  
  coef[, x := rep(x_range, 2)]
  
  coef[, x := ifelse(model == 'Baseline', x - 0.1, x + 0.1)]
  
  coef %>% 
    ggplot(aes(x = x, y = Estimate, color = model, shape = model)) +
    geom_vline(xintercept = (treat_time - 0.5), alpha = 0.5, linetype = 'dashed') + 
    geom_hline(yintercept = 0, size = 0.5, linetype = 'dashed') +
    geom_errorbar(aes(ymin = low, ymax = high), 
                  size = bar_size, 
                  alpha = 0.8,
                  width = 0.6) +
    geom_linerange(aes(ymin = low_sup_t, ymax = high_sup_t), 
                   size = bar_size,
                   alpha = 0.8) +
    geom_point(size = point_size,
               alpha = 0.8) +
    scale_x_continuous(breaks = x_range[c(TRUE, FALSE)]) +
    ggplot_theme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_color_manual(values = c('honeydew4', 'darkorange'),
                       name = 'Model') +
    scale_shape_manual(values = c(17, 19),
                       name = 'Model')
}

