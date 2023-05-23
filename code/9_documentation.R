setwd('~/Dropbox/Work/Research')
figures_folder = '/Users/terencechau/Dropbox/Apps/ShareLaTeX/NASA/figures/documentation/'
source('NASA/replication_package/R/functions.R')

library(scales)

# Set number of cores for parallelization
core_count = detectCores() - 2

# COVERAGE ---------------------------------------------------------------------

hpdf = fread('~/Dropbox/Work/Research/Patents/input/USPTO HPDF/historical_masterfile.csv')
fleming = fread('NASA/input/government_patents/uspto.govt.reliance.metadata.tsv')
cusp = fread('CUSP Berkes Data/patents_fyear_iyear_1940_1980.csv')

hpdf[, year_hpdf := str_sub(disp_dt, start = 6)]
hpdf[, year_hpdf := as.numeric(year_hpdf)]
hpdf[, uspc_sub_hpdf := paste(uspc, uspc_sub, sep = '_')]
hpdf = hpdf[str_detect(uspc, pattern = 'D', negate = TRUE)]

hpdf = hpdf[year_hpdf %in% 1940:1980]
fleming = fleming[grantyear %in% 1940:1980]

hpdf_ids = hpdf %>% 
  filter(patent >= 0) %>% 
  mutate(patent = as.numeric(patent)) %>% 
  select(patent) %>% 
  drop_na() %>% 
  pull(patent) %>% 
  unique()
  
fleming_ids = fleming %>% 
  mutate(patno = as.numeric(patno)) %>% 
  select(patno) %>% 
  drop_na() %>% 
  pull(patno) %>% 
  unique()
  
cusp_ids = cusp %>% 
  filter(iyear %in% 1940:1980) %>% 
  pull(patnum) %>% 
  unique()

length(setdiff(hpdf_ids, fleming_ids))
899/2052239

length(setdiff(hpdf_ids, cusp_ids))
47/2052239

# DATE OVERLAP -----------------------------------------------------------------

setnames(fleming, 'grantyear', 'year_fleming')
setnames(fleming, 'patno', 'patent')
setnames(cusp, 'iyear', 'year_cusp')
setnames(cusp, 'patnum', 'patent')
cusp[, patent := as.character(patent)]

date_overlap = merge(hpdf[, .(patent, year_hpdf)],
                     cusp[, .(patent, year_cusp)],
                     all = FALSE,
                     by = 'patent')
date_overlap = merge(date_overlap,
                     fleming[, .(patent, year_fleming)],
                     all = FALSE,
                     by = 'patent')

date_overlap[, match_hpdf_cusp := year_hpdf == year_cusp]
date_overlap[, match_hpdf_fleming := year_hpdf == year_fleming]
date_overlap[, match_cusp_fleming := year_cusp == year_fleming]

table(date_overlap$match_cusp_fleming)
table(date_overlap$match_hpdf_cusp)
table(date_overlap$match_hpdf_fleming)

# USPC OVERLAP -----------------------------------------------------------------

cusp = fread('CUSP Berkes Data/patents_uspto_categories_1940_1980.csv')
cusp = cusp[main_class == 1]
cusp[, uspc_class := str_replace(uspc_class, '\\/', '_')]
cusp[, patnum := as.character(patnum)]

setnames(cusp, 'patnum', 'patent')
setnames(cusp, 'uspc_class', 'uspc_sub_cusp')

uspc_overlap = merge(hpdf[, .(patent, uspc_sub_hpdf)], 
                     cusp[, .(patent, uspc_sub_cusp)],
                     by = 'patent',
                     all = FALSE)

uspc_overlap[, uspc_sub_cusp := str_replace(uspc_sub_cusp, '\\..*', '')]
uspc_overlap[, uspc_sub_cusp := tolower(uspc_sub_cusp)]
uspc_overlap[, uspc_sub_cusp := str_replace_all(uspc_sub_cusp, '[a-z]*', '')]
uspc_overlap[, match_sub := ifelse(uspc_sub_hpdf == uspc_sub_cusp, 1, 0)]
table(uspc_overlap$match_sub)

uspc_overlap[, uspc_hpdf := str_extract(uspc_sub_hpdf, '.*(?=\\_)')]
uspc_overlap[, uspc_cusp := str_extract(uspc_sub_cusp, '.*(?=\\_)')]
uspc_overlap[, match := ifelse(uspc_hpdf == uspc_cusp, 1, 0)]
table(uspc_overlap$match)

# PATENTING TIME SERIES --------------------------------------------------------

hpdf = fread('~/Dropbox/Work/Research/Patents/input/USPTO HPDF/historical_masterfile.csv')
fleming = fread('NASA/input/government_patents/uspto.govt.reliance.metadata.tsv')
cusp = fread('CUSP Berkes Data/patents_fyear_iyear_1940_1980.csv')

hpdf[, year := str_sub(disp_dt, start = 6)]
hpdf[, year := as.numeric(year)]
hpdf[, uspc_sub_hpdf := paste(uspc, uspc_sub, sep = '_')]
hpdf = hpdf[str_detect(uspc, pattern = 'D', negate = TRUE)]

setnames(fleming, 'grantyear', 'year')
setnames(fleming, 'patno', 'patent')
setnames(cusp, 'iyear', 'year')
setnames(cusp, 'patnum', 'patent')
cusp[, patent := as.character(patent)]

cusp = cusp[year %in% 1940:1980]
cusp = cusp[patent >= 0]

counts_per_year_1 = hpdf[, .(count = .N), by = 'year']
counts_per_year_1[, df := 'HPDF']
counts_per_year_2 = fleming[, .(count = .N), by = 'year']
counts_per_year_2[, df := 'Fleming et al.']
counts_per_year_3 = cusp[, .(count = .N), by = 'year']
counts_per_year_3[, df := 'CUSP']

counts_per_year = rbindlist(list(counts_per_year_1, counts_per_year_2, counts_per_year_3))
rm(counts_per_year_1, counts_per_year_2, counts_per_year_3)

counts_plot = counts_per_year %>% 
  filter(year >= 1900) %>% 
  ggplot(aes(x = year, y = count, color = df, shape = df)) +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) + 
  scale_shape_manual(values = c(15, 17, 19),
                     name = 'Model') +
  scale_color_manual(values = c('#0072B2', '#009E73', '#E69F00'),
                     name = 'Model') +
  ggplot_theme() +
  theme(legend.position = 'bottom') +
  labs(y = 'Count',
       x = 'Year') +
  scale_y_continuous(labels = label_comma())

ggsave(filename = paste0(figures_folder, 'patent_count.pdf'),
       plot = counts_plot,
       height = plot_height,
       width = plot_width)

# CITATIONS --------------------------------------------------------------------

# Load citation data from Fleming
old_citations = fread('NASA/input/government_patents/uspto.citation.1926-1975.tsv')
new_citations = fread('NASA/input/government_patents/uspto.citation.1976-2017.tsv')

# Filter to citations made to US patents
new_citations = new_citations[country %in% c('US', 'USA')]

# Clean and count forward citations for each patent in the Fleming data
old_citations[, id := NULL]
new_citations[, country := NULL]

citations_fleming = rbindlist(list(old_citations, new_citations))
citations_fleming = citations_fleming[str_detect(cited, pattern = '^[:digit:]+$')]
citations_fleming = citations_fleming[str_detect(citing, pattern = '^[:digit:]+$')]
rm(old_citations, new_citations)
gc()

# Load CUSP citation data
citations_cusp = fread('CUSP Berkes Data/cit_given_long.csv')
citations_cusp = citations_cusp[, .(patnum, cit_given_patnum)]
names(citations_cusp) = c('citing', 'cited')

setorder(citations_fleming, citing, cited)
setorder(citations_cusp, citing, cited)

hpdf = fread('~/Dropbox/Work/Research/Patents/input/USPTO HPDF/historical_masterfile.csv')
first_1940_patent = hpdf %>% 
  filter(str_detect(disp_dt, 'jan1940')) %>% 
  arrange(disp_dt, patent) %>% 
  slice_head(n = 1) %>% 
  pull(patent) %>% 
  as.numeric()
last_1980_patent = hpdf %>% 
  filter(str_detect(disp_dt, 'dec1980')) %>% 
  arrange(disp_dt, patent) %>% 
  slice_tail(n = 1) %>% 
  pull(patent) %>% 
  as.numeric()

citations_cusp = citations_cusp[citing >= first_1940_patent &
           citing <= last_1980_patent &
           cited >= first_1940_patent &
           cited <= last_1980_patent]

# Subset the Fleming data to the CUSP subsample
citations_fleming[, citing := as.numeric(citing)]
citations_fleming[, cited := as.numeric(cited)]
citations_fleming_small = citations_fleming[citing >= first_1940_patent &
                                              citing <= last_1980_patent &
                                              cited >= first_1940_patent &
                                              cited <= last_1980_patent]
citations_cusp = unique(citations_cusp)
citations_fleming_small = unique(citations_fleming_small)

# Check overlap in citings
citing_diff_1 = setdiff(citations_fleming_small$citing, citations_cusp$citing)
citing_diff_2 = setdiff(citations_cusp$citing, citations_fleming_small$citing)

summary(citing_diff_2)

length(unique(citations_cusp$citing))
length(unique(citations_fleming_small$citing))

# Check overlap in citeds
cited_diff_1 = setdiff(citations_fleming_small$cited, citations_cusp$cited)
cited_diff_2 = setdiff(citations_cusp$cited, citations_fleming_small$cited)

summary(cited_diff_2)

length(unique(citations_cusp$cited))
length(unique(citations_fleming_small$cited))

# Count citation count delta for patents in both datasets
citation_count_cusp = citations_cusp[, .(citing_count_cusp = .N), by = cited]
citation_count_fleming = citations_fleming_small[, .(citing_count_fleming = .N), by = cited]

citation_count_overlap = merge(citation_count_cusp, 
                               citation_count_fleming,
                               by = 'cited',
                               all = FALSE)
citation_count_overlap[, difference := abs(citing_count_cusp - citing_count_fleming)]
citation_count_overlap[, difference_bin := as.factor(
  case_when(
    difference == 0 ~ '0',
    difference %in% 1:5 ~ '[1, 5]',
    difference %in% 6:10 ~ '[6, 10]',
    difference %in% 11:20 ~ '[11, 20]',
    difference > 20 ~ '[21, 43]',
    TRUE ~ as.character(NA))
)]

citation_count_overlap %>% 
  ggplot(aes(x = difference_bin)) +
  geom_histogram(stat = 'count')

table(citation_count_overlap$difference_bin)
