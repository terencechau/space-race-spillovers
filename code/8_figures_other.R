# This script generates miscellaneous figures in Public R&D and Spillovers: 
# Evidence from NASA Patents (Chau, 2023)

setwd('~/Dropbox/Work/Research/')
figures_folder = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/figures/'

# Load packages and functions
source('NASA/replication_package/R/functions.R')


# BUDGETS ----------------------------------------------------------------------


# Plot NASA R&D outlays, alone, compared to other fields, in current dollars
# and as shares of GDP, total federal outlays, or total federal R&D outlays
budget = read_excel('NASA/input/budget_data.xlsx')
budget %<>% 
  filter(year != 'TQ') %>% 
  mutate(year = as.numeric(year))

budget_long = pivot_longer(budget,
                           cols = -c(year, outlays_total, matches('gdp'),
                                     total_rnd, cpi),
                           names_to = 'category',
                           values_to = 'outlay')

reference_year = 2020
reference_cpi = budget %>% 
  filter(year == reference_year) %>% 
  pull(cpi) %>% 
  unique()
budget_long %<>% mutate(deflator = reference_cpi/cpi)
budget_long %<>% 
  mutate(across(-c(year, category, outlays_total, matches('gdp'), matches('cpi'), 
                   total_rnd, deflator), 
                list(share_gdp = ~  .x/gdp,
                     share_total_outlays = ~ .x/outlays_total,
                     share_rnd = ~ .x/total_rnd)))
budget_long %<>% 
  mutate(across(-c(year, category, matches('cpi'), deflator, matches('perc')),
                list(constant = ~ deflator * .x)))

# Convert to billions
budget_long %<>% mutate(outlay_constant_billions = outlay_constant/1000)

find_axis_max = function(x, cat, digits) {
  budget_long %>% 
    filter(category %in% cat) %>% 
    pull(x) %>% 
    max(na.rm = TRUE) %>% 
    ceiling_decimal(digits)
}


# NASA OUTLAYS IN CONSTANT 2020 DOLLARS ----------------------------------------


y_var = 'outlay_constant_billions'
cat_var = 'nasa_total'
y_max = find_axis_max(x = y_var, cat = cat_var, digits = 0)

budget_plot = budget_long %>% 
  select(year, all_of(y_var), category) %>% 
  filter(category %in% cat_var) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = .data[[y_var]])) +
  geom_rect(aes(xmin = 1958, xmax = 1972, ymin = -Inf, ymax = Inf), fill= 'gray60', alpha = 0.01) +
  geom_line(color = 'darkorange', linetype = 'dashed') + 
  geom_line(aes(x = year, y = .data[[y_var]]),
            data = . %>% filter(year >= 1958),
            color = 'darkorange') +
  ggplot_theme() +
  scale_x_continuous(breaks = seq(1948, 2022, 5)) +
  scale_y_continuous(breaks = seq(0, y_max, 5)) +
  xlab('Year') +
  ylab('Constant 2020 Dollars (Billions)')

budget_plot

ggsave(filename = paste0(figures_folder, 'budget/nasa_outlay_billions.pdf'), 
       plot = budget_plot,
       width = plot_width + 1,
       height = plot_height)   


# NASA VS ALL OUTLAY IN BILLIONS AND AS SHARE OF R&D ---------------------------


y_var = 'outlay_constant_billions'
cat_var = c('defense_total', 'science_space_technology_total', 'health_total',
            'energy', 'transportation_total', 'agriculture', 
            'natural_resources_environment', 'grants_nondefense')

budget_plot = budget_long %>% 
  mutate(category = factor(category,
                           levels = c('defense_total',
                                      'science_space_technology_total',
                                      'health_total',
                                      'energy',
                                      'transportation_total',
                                      'natural_resources_environment',
                                      'agriculture',
                                      'grants_nondefense'))) %>% 
  select(year, all_of(y_var), category) %>% 
  filter(category %in% cat_var) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = .data[[y_var]], color = category, 
             fill = after_scale(color))) +
  geom_area() + 
  ggplot_theme() +
  scale_color_manual(values = c('#BBBBBB', '#CCBB44','#666633','#994455', 
                                        '#CC6677', '#66CCEE', '#4477AA', '#999933'),
                                         labels = c('Defense', 
                                                    'Science, Space, & Technology',
                                                    'Health', 'Energy', 
                                                    'Transportation',
                                                    'Environment', 'Agriculture',
                                                    'Nondefense Grants'),
                     name = 'Category') +
  scale_x_continuous(breaks = seq(1948, 2022, 5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab('Year') +
  ylab('Constant 2020 Dollars (Billions)') 


y_var = 'outlay_share_rnd'
budget_plot_2 = budget_long %>% 
  mutate(category = factor(category,
                           levels = c('defense_total',
                                      'science_space_technology_total',
                                      'health_total',
                                      'energy',
                                      'transportation_total',
                                      'natural_resources_environment',
                                      'agriculture',
                                      'grants_nondefense'))) %>% 
  select(year, all_of(y_var), category) %>% 
  filter(category %in% cat_var) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = .data[[y_var]], color = category, 
             fill = after_scale(color))) +
  geom_area() + 
  ggplot_theme() +
  scale_color_manual(values = c('#BBBBBB', '#CCBB44','#666633','#994455', 
                                        '#CC6677', '#66CCEE', '#4477AA', '#999933'),
                                        labels = c('Defense', 
                                                   'Science, Space, & Technology',
                                                   'Health', 'Energy', 
                                                   'Transportation',
                                                   'Environment', 'Agriculture',
                                                   'Nondefense Grants'),
                     name = 'Category') +
  scale_x_continuous(breaks = seq(1948, 2022, 5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab('Year') +
  ylab('Percentage') 

budget_plot = ggarrange(budget_plot, budget_plot_2, ncol = 2, nrow = 1, common.legend = TRUE, legend = 'bottom')
budget_plot

# ggsave(filename = paste0(figures_folder, 'budget/federal_rnd_composition.pdf'), 
#        plot = budget_plot,
#        width = 8,
#        height = 4.5)

# Save separately for slides
budget_plot_short = budget_long %>% 
  filter(year %in% 1940:1980) %>% 
  mutate(category = factor(category,
                           levels = c('defense_total',
                                      'science_space_technology_total',
                                      'health_total',
                                      'energy',
                                      'transportation_total',
                                      'natural_resources_environment',
                                      'agriculture',
                                      'grants_nondefense'))) %>% 
  select(year, all_of(y_var), category) %>% 
  filter(category %in% cat_var) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = .data[[y_var]], color = category, 
             fill = after_scale(color))) +
  geom_area() + 
  ggplot_theme() +
  scale_color_manual(values = c('#BBBBBB', '#CCBB44','#666633','#994455', 
                                         '#CC6677', '#66CCEE', '#4477AA', '#999933'),
                                         labels = c('Defense', 
                                                    'Science, Space, & Technology',
                                                    'Health', 'Energy', 
                                                    'Transportation',
                                                    'Environment', 'Agriculture',
                                                    'Nondefense Grants'),
                     name = 'Category') +
  scale_x_continuous(breaks = seq(1948, 1980, 5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab('Year') +
  ylab('Constant 2020 Dollars (Billions)') 

ggsave(filename = paste0(figures_folder, 'budget/federal_rnd_outlay_1980.pdf'), 
       plot = budget_plot_short,
       width = plot_width + 1,
       height = plot_height)

budget_plot_2_short = budget_long %>% 
  filter(year %in% 1940:1980) %>% 
  mutate(category = factor(category,
                           levels = c('defense_total',
                                      'science_space_technology_total',
                                      'health_total',
                                      'energy',
                                      'transportation_total',
                                      'natural_resources_environment',
                                      'agriculture',
                                      'grants_nondefense'))) %>% 
  select(year, all_of(y_var), category) %>% 
  filter(category %in% cat_var) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = .data[[y_var]], color = category, 
             fill = after_scale(color))) +
  geom_area() + 
  ggplot_theme() +
  scale_color_manual(values = c('#BBBBBB', '#CCBB44','#666633','#994455', 
                                         '#CC6677', '#66CCEE', '#4477AA', '#999933'),
                                         labels = c('Defense', 
                                                    'Science, Space, & Technology',
                                                    'Health', 'Energy', 
                                                    'Transportation',
                                                    'Environment', 'Agriculture',
                                                    'Nondefense Grants'),
                     name = 'Category') +
  scale_x_continuous(breaks = seq(1948, 2022, 5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab('Year') +
  ylab('Percentage') 

ggsave(filename = paste0(figures_folder, 'budget/federal_rnd_share_1980.pdf'), 
       plot = budget_plot_2_short,
       width = plot_width + 1,
       height = plot_height)


# SIMILAR PLOTS NOT IN PAPER ---------------------------------------------------


# NASA OUTLAYS AS SHARE OF RND --------------------------------------------------


y_var = 'outlay_share_rnd'
cat_var = 'nasa_total'
y_max = find_axis_max(x = y_var, cat = cat_var, digits = 1)

budget_plot = budget_long %>% 
  select(year, all_of(y_var), category) %>% 
  filter(category %in% cat_var) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = .data[[y_var]])) +
  geom_line(color = 'darkorange') + 
  ggplot_theme() +
  scale_x_continuous(breaks = seq(1948, 2022, 5)) +
  scale_y_continuous(breaks = seq(0, y_max, 0.05),
                     labels = scales::percent) +
  xlab('Year') +
  ylab('Percentage')

# ggsave(filename = paste0(figures_folder, 'budget/nasa_outlay_share_rnd.pdf'), 
#        plot = budget_plot,
#        width = plot_width,
#        height = plot_height)    


# NASA OUTLAYS AS SHARE OF GDP --------------------------------------------------


y_var = 'outlay_share_gdp'
cat_var = 'nasa_total'
y_max = find_axis_max(x = y_var, cat = cat_var, digits = 2)

budget_plot = budget_long %>% 
  select(year, all_of(y_var), category) %>% 
  filter(category %in% cat_var) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = .data[[y_var]], color = category)) +
  geom_line(color = 'darkorange') + 
  ggplot_theme() +
  scale_x_continuous(breaks = seq(1948, 2022, 5)) +
  scale_y_continuous(breaks = seq(0, y_max, 0.001),
                     labels = scales::percent) +
  xlab('Year') +
  ylab('Percentage') 

# ggsave(filename = paste0(figures_folder, 'budget/nasa_outlay_share_gdp.pdf'), 
#        plot = budget_plot,
#        width = plot_width,
#        height = plot_height)    


# NASA VS HEALTH BUDGET CONSTANT 2020 DOLLARS ----------------------------------


y_var = 'outlay_constant_billions'
cat_var = c('nasa_total', 'health_total')
y_max = find_axis_max(x = y_var, cat = cat_var, digits = 0)

budget_plot = budget_long %>% 
  select(year, all_of(y_var), category) %>% 
  filter(category %in% cat_var) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = .data[[y_var]], color = category)) +
  geom_line() + 
  ggplot_theme() +
  scale_color_manual(values = c('honeydew4', 'darkorange'),
                     labels = c('Health R&D', 'NASA R&D'),
                     name = 'Category') +
  scale_x_continuous(breaks = seq(1948, 2022, 5)) +
  scale_y_continuous(breaks = seq(0, y_max, 5)) +
  xlab('Year') +
  ylab('Constant 2020 Dollars (Billions)') 

# ggsave(filename = paste0(figures_folder, 'budget/nasa_vs_health_outlay_billions.pdf'), 
#        plot = budget_plot,
#        width = plot_width,
#        height = plot_height)      


# STEM ENROLLMENTS -------------------------------------------------------------


enrollments = read_excel('NASA/input/college_enrollments.xlsx')

colnames(enrollments) %<>% tolower()

enrollments %<>%
  pivot_longer(values_to = 'enrollment',
               names_to = 'field',
               cols = -c(year, total))

enrollments %<>% 
  mutate(year = str_replace(year, 
                            pattern = '-[0-9]*',
                            replacement = ''),
         year = as.numeric(year))

enrollment_plot = enrollments %>% 
  filter(field %in% c('computer and information sciences')) %>% 
  ggplot(aes(x = year, y = enrollment, color = field)) +
  geom_line(color = 'darkorange') +
  ggplot_theme() +
  labs(x = 'Year',
       y = 'Enrollment') +
  scale_x_continuous(breaks = seq(1960, 1990, 5))

# ggsave(filename = paste0(figures_folder, 'enrollments/cs_enrollments.pdf'), 
#        plot = enrollment_plot,
#        width = plot_width,
#        height = plot_height)


# MOST COMMON NASA USPC CLASSES ------------------------------------------------

df = fread('NASA/final_output/diff_in_means_df.csv')
classes = fread('Patents/input/uspc_classes.csv')
df[, uspc := str_extract(string = uspc_sub, pattern = '([0-9]*)(?=_)')]
df = merge(df, classes, by = 'uspc')

nasa_broad = df[nasa == 1, .(count = .N), by = .(uspc, uspc_name)]
nasa_narrow = df[nasa == 1, .(count = .N), by = .(uspc_sub, uspc, uspc_name)]
setorder(nasa_broad, -count)
setorder(nasa_narrow, -count)

top_nasa_broad = head(nasa_broad, 10)
top_nasa_narrow = head(nasa_narrow, 10)

# Clean up names for plots
top_nasa_broad[, uspc_name := str_to_title(uspc_name)]
top_nasa_broad[, uspc_name := ifelse(uspc_name == 'Chemistry: Electrical Current Producing Apparatus, Product, And Process',
                                     'Chemistry: Electrical Current Apparatus',
                                     uspc_name)]
top_nasa_broad[, uspc_name := str_replace(uspc_name, 'And', '&')]
top_nasa_broad[, uspc_name := str_replace(uspc_name, 'Miscellaneous', 'Misc.')]

# Add narrow names
#244_172 Aeronautics and Astronautics, Spacecraft, With Special Crew Accomodations, Emergency Rescue Means
#423_447 Chemistry of Inorganic Compounds, Carbon or Compound Thereof, Fiber, Fabric or Textile
#60_202 Power Plants, Reaction Motor, (e.g. Motive Fluid Generator and Reaction Nozzle), Ion Motor
#73_147 Measuring and Testing, Wind Tunnel, Aerodynamic Wing and Propeller Study
#244_158 Aeronautics and Astronautics, Spacecraft, General
#250_208 Radiant Energy, Photocells, Circuits and Apparatus
#73_862 Measuring and Testing, Dynamometers
#73_1 Measuring and Testing, Instrument Proving or Calibrating
#73_861 Measuring and Testing, Volume or Rate of Flow
#244_171 Aeronautics and Astronautics, Spacecraft, Attitude Control, Attitude Sensor Means

uspc_sub_names = c('Aeronautics and Astronautics: Spacecraft Emergency Rescue Means',
                   'Inorganic Chemistry: Carbon or Compound Thereof, Fiber, Fabric or Textile',
                   'Power Plants: Reaction Motor, Ion Motor',
                   'Measuring and Testing: Wind Tunnel, Aerodynamic Study',
                   'Aeronautics and Astronautics: Spacecraft, General',
                   'Radiant Energy: Photocells, Circuits & Apparatus',
                   'Measuring and Testing: Dynamometers',
                   'Measuring and Testing: Instrument Proving or Calibrating',
                   'Measuring and Testing: Volume or Rate of Flow',
                   'Aeronautics and Astronautics: Spacecraft, Attitude Sensor Means')
top_nasa_narrow[, uspc_sub_names := uspc_sub_names]

top_nasa_broad_plot = top_nasa_broad %>% 
  ggplot(aes(y = count, 
             x = reorder(uspc_name, count))) +
  geom_bar(fill = 'aquamarine4', stat = 'identity') +
  ggplot_theme() +
  labs(y = 'Count', x = 'USPC Broad Class') +
  coord_flip()

ggsave(filename = paste0(figures_folder, 'top_classes/broad.pdf'),
       plot = top_nasa_broad_plot,
       width = plot_width + 2,
       height = plot_height)


top_nasa_narrow_plot = top_nasa_narrow %>% 
  ggplot(aes(y = count, 
             x = reorder(uspc_sub_names, count))) +
  geom_bar(fill = 'aquamarine4', stat = 'identity') +
  ggplot_theme() +
  labs(y = 'Count', x = 'USPC Narrow Class') +
  coord_flip()

ggsave(filename = paste0(figures_folder, 'top_classes/narrow.pdf'),
       plot = top_nasa_narrow_plot,
       width = plot_width + 4,
       height = plot_height)

  