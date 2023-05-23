# This script takes the Fleming et al. (2019) data and the USPTO HPDF (Marco et al, 2015)
# data to construct a technology subclass by year panel dataset for the main analysis in
# Public R&D and Spillovers: Evidence from NASA Patents (Chau, 2023)

setwd('~/Dropbox/Work/Research')
source('NASA/replication_package/R/functions.R')

# Set number of cores for parallelization
core_count = detectCores() - 2

# Load Fleming data, which contains federal reliance attribution for each patent
# Metadata specifies the federal agency involved
# Category details the type of federal reliance
fleming_data = fread('NASA/input/government_patents/uspto.govt.reliance.metadata.tsv')
fleming_data_2 = fread('NASA/input/government_patents/uspto.govt.reliance.category.tsv')
colnames(fleming_data) %<>% tolower()
colnames(fleming_data_2) %<>% tolower()

# Save patents that are directly owned or contracted by a federal agency
category_1_2 = fleming_data_2 %>% 
  filter(category_1_owned_by_usgovt == 1|category_2_acknowledge_usgovt_support == 1)
fleming_data %<>% 
  filter(patno %in% category_1_2$patno)
rm(category_1_2)
gc()

# Load HPDF data, which contains all utility patents issued and their technology classification
all_patents = fread('~/Dropbox/Work/Research/Patents/input/USPTO HPDF/historical_masterfile.csv')

# Drop patents with non-valid USPC codes
all_patents = all_patents[!(uspc %in% c('', '000', 'PLT', 'XXX', '999'))]

# Drop design patents
all_patents = all_patents[str_detect(uspc, pattern = 'D', negate = TRUE)]

# Drop patents with non-valid USPC subclass codes
all_patents = all_patents[!(uspc_sub %in% c('', 'PCT', 'XXX'))]

setnames(fleming_data, 'patno', 'patent_id')
setnames(all_patents, 'patent', 'patent_id')
all_patents[, uspc_sub_long := paste(uspc, uspc_sub, sep = '_')]
all_patents[, year_issue := str_sub(disp_dt, start = 6)]

# Add technology class to government patents
govt_patents = merge(fleming_data[, .(patent_id, grantyear, dod, 
                                      doe, nasa, nsf, hhs, usda, others)], 
                     all_patents[, .(patent_id, uspc, uspc_sub_long)], 
                     all.x = TRUE, by = 'patent_id')


# GET CITATIONS-----------------------------------------------------------------


# Prepare citation data

# Load citation data from Fleming
old_citations = fread('NASA/input/government_patents/uspto.citation.1926-1975.tsv')
new_citations = fread('NASA/input/government_patents/uspto.citation.1976-2017.tsv')

# Filter to citations made to US patents
new_citations = new_citations[country %in% c('US', 'USA')]

# Clean and count forward citations for each patent in the Fleming data
old_citations[, id := NULL]
new_citations[, country := NULL]

citations = rbindlist(list(old_citations, new_citations))
citations = citations[str_detect(cited, pattern = '^[:digit:]+$')]
citations = citations[str_detect(citing, pattern = '^[:digit:]+$')]

# Identify if citing or cited patents are NASA
citations[, citing_nasa := ifelse(citing %in% fleming_data[nasa > 0]$patent_id, 1, 0)]
citations[, cited_nasa := ifelse(cited %in% fleming_data[nasa > 0]$patent_id, 1, 0)]

# Merge cited patent year of issue and uspc into citations data from HPDF file
setnames(all_patents, c('patent_id', 'year_issue', 'uspc', 'uspc_sub_long'), 
         c('cited', 'cited_year', 'cited_uspc', 'cited_uspc_sub'))
citations = merge(citations, all_patents[, .(cited, cited_year, cited_uspc, cited_uspc_sub)], 
                  by = 'cited', all.x = TRUE)

# Merge citing patent year of issue and uspc into citations data from HPDF file
setnames(all_patents, c('cited', 'cited_year', 'cited_uspc', 'cited_uspc_sub'), 
         c('citing', 'citing_year', 'citing_uspc', 'citing_uspc_sub'))
citations = merge(citations, all_patents[, .(citing, citing_year, citing_uspc, citing_uspc_sub)], 
                  by = 'citing', all.x = TRUE)
citations[, citation_delta := as.numeric(citing_year) - as.numeric(cited_year)]

# Reset names in all patents
setnames(all_patents, c('citing', 'citing_year', 'citing_uspc', 'citing_uspc_sub'), 
         c('patent_id', 'year_issue', 'uspc', 'uspc_sub_long'))

# Count citations at the subclass by year level
# For each citation outcome, count: total, total excluding NASA, leave one out
# subclass (LOO), leave one out excluding NASA, leave one out broad class (LOO B),
# leave one out broad excluding NASA.
# Also count number of other subclasses citing reference class
citations[, cond_1 := !(cited_nasa == 1 & citing_nasa == 1)]
citations[, cond_2 := !(cited_uspc_sub == citing_uspc_sub)]
citations[, cond_3 := !(cited_uspc == citing_uspc)]
citations[, cond_4 := citation_delta %in% 0:20]

# Fix citation deltas (assume zero if negative)
citations[, citation_delta := ifelse(citation_delta < 0, 0, citation_delta)]

# Lifetime citations to patents in subclass issued in year and
# Citations within 20 years to patents in subclass issued in year
citations_df_1 = citations[,
                           .(citations_lifetime = .N,
                             citations_lifetime_excl_nasa = sum(cond_1, na.rm = TRUE),
                             citations_lifetime_loo = sum(cond_2, na.rm = TRUE),
                             citations_lifetime_loo_excl_nasa = sum(cond_1 & cond_2, na.rm = TRUE),
                             citations_lifetime_loo_b = sum(cond_3, na.rm = TRUE),
                             citations_lifetime_loo_b_excl_nasa = sum(cond_1 & cond_3, na.rm = TRUE),
                             citations_20_yr = sum(cond_4, na.rm = TRUE),
                             citations_20_yr_excl_nasa = sum(cond_1 & cond_4, na.rm = TRUE),
                             citations_20_yr_loo = sum(cond_2 & cond_4, na.rm = TRUE),
                             citations_20_yr_loo_excl_nasa = sum(cond_1 & cond_2 & cond_4, na.rm = TRUE),
                             citations_20_yr_loo_b = sum(cond_3 & cond_4, na.rm = TRUE),
                             citations_20_yr_loo_b_excl_nasa = sum(cond_1 & cond_3 & cond_4, na.rm = TRUE),
                             citations_lifetime_uspc_count = ifelse(!is_empty(length(setdiff(unique(citing_uspc_sub), NA))),
                                                                    length(setdiff(unique(citing_uspc_sub), NA)),
                                                                    NA),
                             citations_lifetime_loo_uspc_share = ifelse(.N != 0,
                                                                        sum(cond_2, na.rm = TRUE)/.N,
                                                                        NA),
                             citations_lifetime_loo_b_uspc_share = ifelse(.N != 0,
                                                                         sum(cond_3, na.rm = TRUE)/.N,
                                                                         NA),
                             citations_mean_lag = ifelse(sum(!is.na(citation_delta)) != 0,
                                                       mean(citation_delta, na.rm = TRUE),
                                                       as.numeric(NA)),
                             citations_max_lag = ifelse(sum(!is.na(citation_delta)) != 0,
                                                        max(citation_delta, na.rm = TRUE),
                                                        as.numeric(NA))
                             # citations_mean_lag_norm = ifelse(sum(!is.na(citation_delta)) != 0 & 
                             #                                    sd(citation_delta, na.rm = TRUE) != 0 & 
                             #                                    !is.na(citations_mean_lag),
                             #                                  citations_mean_lag/sd(citation_delta, na.rm = TRUE),
                             #                                  as.numeric(NA))
                             # citations_max_lag_norm = ifelse(sum(!is.na(citation_delta)) != 0,
                             #                                 max(citation_delta, na.rm = TRUE)/sd(citation_delta, na.rm = TRUE),
                             #                                 as.numeric(NA))
                           ),
                           by = .(cited_uspc_sub, cited_year)]

# Lifetime citation HHI
cited_uspcs = unique(citations$cited_uspc_sub)
citations_lifetime_hhi = lapply(cited_uspcs, FUN = function(i) {
    citation_shares = citations[cited_uspc_sub == i,
                                .(citations_from_class = .N),
                                by = .(cited_year, citing_uspc_sub)]
    citation_shares[, total_citations := sum(citations_from_class, na.rm = TRUE),
                    by = .(cited_year)]
    citation_shares[, citation_share := ifelse(total_citations != 0,
                                               citations_from_class/total_citations,
                                               NA),
                    by = .(cited_year)]
    citation_shares[, citation_share_sq := citation_share^2,
                    by = .(cited_year)]
    citations_lifetime_hhi = citation_shares[, .(generality_lifetime_hhi = 1 - sum(citation_share_sq, na.rm = TRUE)),
                                            by = .(cited_year)]
    citations_lifetime_hhi[, cited_uspc_sub := i]
})

citations_lifetime_hhi %<>% bind_rows()
citations_df_1 = merge(citations_df_1, citations_lifetime_hhi, by = c('cited_uspc_sub', 'cited_year'), all.x = TRUE)

# Citations made in year to patents in subclass of any year
citations_df_2 = citations[,
                           .(citations_year = .N, 
                             citations_year_excl_nasa = sum(cond_1, na.rm = TRUE),
                             citations_year_loo = sum(cond_2, na.rm = TRUE),
                             citations_year_loo_excl_nasa = sum(cond_1 & cond_2, na.rm = TRUE),
                             citations_year_loo_b = sum(cond_3, na.rm = TRUE),
                             citations_year_loo_b_excl_nasa = sum(cond_1 & cond_3, na.rm = TRUE),
                             citations_year_uspc_count = ifelse(!is_empty(length(setdiff(unique(citing_uspc_sub), NA))),
                                                                length(setdiff(unique(citing_uspc_sub), NA)),
                                                                NA),
                             citations_year_loo_uspc_share = ifelse(.N != 0,
                                                                    sum(cond_2, na.rm = TRUE)/.N,
                                                                    NA),
                             citations_year_loo_b_uspc_share = ifelse(.N != 0,
                                                                      sum(cond_3, na.rm = TRUE)/.N,
                                                                      NA)
                             ), 
                           by = .(citing_year, cited_uspc_sub)]

# Yearly citation HHI
citations_year_hhi = lapply(cited_uspcs, FUN = function(i) {
  citation_shares = citations[cited_uspc_sub == i,
                              .(citations_from_class = .N),
                              by = .(citing_year, citing_uspc_sub)]
  citation_shares[, total_citations := sum(citations_from_class, na.rm = TRUE),
                  by = .(citing_year)]
  citation_shares[, citation_share := ifelse(total_citations != 0,
                                             citations_from_class/total_citations,
                                             NA),
                  by = .(citing_year)]
  citation_shares[, citation_share_sq := citation_share^2,
                  by = .(citing_year)]
  citation_lifetime_hhi = citation_shares[, .(generality_year_hhi = 1 - sum(citation_share_sq, na.rm = TRUE)),
                                          by = .(citing_year)]
  citation_lifetime_hhi[, cited_uspc_sub := i]
})

citations_year_hhi %<>% bind_rows()
citations_df_2 = merge(citations_df_2, citations_year_hhi, by = c('cited_uspc_sub', 'citing_year'), all.x = TRUE)

# Breakthrough citation calculation
breakthrough_df = all_patents[year_issue %in% 1948:1980, .(patent_id, uspc_sub_long, year_issue)]
setnames(breakthrough_df, c('uspc_sub_long', 'year_issue'), c('uspc_sub', 'year'))
breakthrough_df[, nasa := ifelse(patent_id %in% fleming_data[nasa > 0]$patent_id, 1, 0)]
breakthrough_df = merge(breakthrough_df, citations[, .(citing, cited)],
                        all.x = TRUE, by.x = 'patent_id', by.y = 'cited')
breakthrough_df[, citation := ifelse(!is.na(citing), 1, 0)]
breakthrough_df = breakthrough_df[, .(citations_lifetime = sum(citation)), by = .(patent_id, uspc_sub, year)]
breakthrough_df[, year := as.numeric(year)]
breakthrough_df = breakthrough_df[!is.na(year)]

breakthrough_regression = felm(citations_lifetime ~ 1|uspc_sub + year, data = breakthrough_df)

# Get percentiles
breakthrough_percentiles = breakthrough_regression$residuals %>% quantile(probs = c(0.9, 0.95, 0.99))

# Define if each patent is a breakthrough or not
breakthrough_df[, citation_residual := breakthrough_regression$residuals]
breakthrough_df[, breakthrough_90 := ifelse(citation_residual >= breakthrough_percentiles[[1]], 1, 0)]
breakthrough_df[, breakthrough_95 := ifelse(citation_residual >= breakthrough_percentiles[[2]], 1, 0)]
breakthrough_df[, breakthrough_99 := ifelse(citation_residual >= breakthrough_percentiles[[3]], 1, 0)]

# Define if each subclass-year cell contains a breakthrough or not
breakthrough_subclasses = breakthrough_df[, .(breakthrough_90 = ifelse(any(breakthrough_90 == 1), 1, 0),
                                              breakthrough_95 = ifelse(any(breakthrough_95 == 1), 1, 0),
                                              breakthrough_99 = ifelse(any(breakthrough_99 == 1), 1, 0)),
                                          by = .(uspc_sub, year)]

# Fix names and merge into one df
# Note from now on, uspc_sub refers to the cited uspc_sub
setnames(citations_df_1, 'cited_year', 'year')
setnames(citations_df_2, 'citing_year', 'year')
citations_df = merge(citations_df_1, citations_df_2,
                     all = TRUE, by = c('cited_uspc_sub', 'year'))

breakthrough_subclasses[, year := as.character(year)]
setnames(breakthrough_subclasses, 'uspc_sub', 'cited_uspc_sub')
citations_df = merge(citations_df, breakthrough_subclasses,
                     all.x = TRUE,
                     by = c('cited_uspc_sub', 'year'))

citations_df = citations_df[!is.na(cited_uspc_sub) & !is.na(year) & year != '' & cited_uspc_sub != '']
citations_df[, year := as.numeric(year)]
setnames(citations_df, 'cited_uspc_sub', 'uspc_sub')
setorder(citations_df, uspc_sub, year)


# GET COUNTS--------------------------------------------------------------------


# Counts by issue year
counts_issued = all_patents[, .(issued = .N), by = .(uspc_sub_long, year_issue)]
counts_issued[, year_issue := as.numeric(year_issue)]
setnames(counts_issued, 'year_issue', 'year')
counts_issued = counts_issued[!is.na(year) & !is.na(uspc_sub_long)]
setorder(counts_issued, uspc_sub_long, year)

# Counts by issue year without NASA
nasa_patent_ids = fleming_data[nasa > 0, .(patent_id)]
counts_issued_excl_nasa = all_patents[!(patent_id %in% nasa_patent_ids$patent_id), 
                                      .(issued_excl_nasa = .N), 
                                      by = .(uspc_sub_long, year_issue)]
setnames(counts_issued_excl_nasa, 'year_issue', 'year')
counts_issued_excl_nasa = counts_issued_excl_nasa[!is.na(year) & !is.na(uspc_sub_long)]
setorder(counts_issued_excl_nasa, uspc_sub_long, year)

# Counts by application year (using Enrico's data)
cusp_years = fread('CUSP Berkes Data/patents_fyear_iyear_1940_1980.csv')
setnames(cusp_years, 'patnum', 'patent_id')
cusp_years[, patent_id := as.character(patent_id)]
cusp_years = merge(cusp_years, all_patents[, .(patent_id, uspc_sub_long)], 
                   by = 'patent_id', all.x = TRUE)
counts_applied = cusp_years[, .(applied = .N), by = .(uspc_sub_long, fyear)]
setnames(counts_applied, 'fyear', 'year')

counts_issued_excl_nasa[, year := as.numeric(year)]
counts_df = merge(counts_issued, counts_issued_excl_nasa, 
                  by = c('uspc_sub_long', 'year'), all = TRUE)
counts_df = merge(counts_df, counts_applied, 
                  by = c('uspc_sub_long', 'year'), all = TRUE)
setnames(counts_df, 'uspc_sub_long', 'uspc_sub')
counts_df = counts_df[!is.na(uspc_sub)]
setorder(counts_df, uspc_sub, year)


# GET FIRST YEAR FOR EACH USPC_SUB----------------------------------------------


all_patents[, year_issue := as.numeric(year_issue)]
first_year = all_patents[, .(first_year = min(year_issue, na.rm = TRUE)), by = uspc_sub_long]
setnames(first_year, 'uspc_sub_long', 'uspc_sub')


# CREATE MAIN EVENT STUDY DF ---------------------------------------------------


# Assemble control group and DOD/NASA overlaps
govt_classes = govt_patents[grantyear %in% 1958:1972, 
                            .(dod = sum(ifelse(dod > 0, 1, 0)),
                              nasa = sum(ifelse(nasa > 0, 1, 0)),
                              doe = sum(ifelse(doe > 0, 1, 0)),
                              nsf = sum(ifelse(nsf > 0, 1, 0)),
                              hhs = sum(ifelse(hhs > 0, 1, 0)),
                              usda = sum(ifelse(usda > 0, 1, 0)),
                              others = sum(ifelse(others > 0, 1, 0))),
                            by = uspc_sub_long]
govt_classes = govt_classes[!(dod == 0 & nasa == 0 & doe == 0 & nsf == 0 & hhs == 0 &
                                usda == 0 & others == 0)]
govt_classes_pre = govt_patents[grantyear %in% 1948:1958, 
                                .(dod = sum(ifelse(dod > 0, 1, 0)),
                                  doe = sum(ifelse(doe > 0, 1, 0)),
                                  nsf = sum(ifelse(nsf > 0, 1, 0)),
                                  hhs = sum(ifelse(hhs > 0, 1, 0)),
                                  usda = sum(ifelse(usda > 0, 1, 0)),
                                  others = sum(ifelse(others > 0, 1, 0))),
                                by = uspc_sub_long]
govt_classes_pre = govt_classes[!(dod == 0 & doe == 0 & nsf == 0 & hhs == 0 &
                                    usda == 0 & others == 0)]

nasa_classes = govt_classes[nasa > 0]
nasa_dod_overlap = nasa_classes[dod > 0]

# Prepare regression sample:
# 1. Subset all data from 1948 to 1980
# 2. Narrow technology classes that were present before 1958
# 3. Classes that some government agency worked on before 1958 (control) and NASA classes 
# worked on 1958 to 1972 (treatment).
# 4. Classes that do already exist get NA filled with zero but only after their existence year
# (because the class is confirmed to exist, but not all years will get a patent)
event_study_df_1 = merge(citations_df, counts_df, 
                         by = c('uspc_sub', 'year'), all = TRUE)

event_study_df_1 = event_study_df_1[year %in% 1948:1980 &
                                    uspc_sub %in% first_year[first_year <= 1958]$uspc_sub &
                                    (uspc_sub %in% govt_classes_pre$uspc_sub_long|
                                     uspc_sub %in% nasa_classes$uspc_sub_long)]
event_study_df_2 = event_study_df_1 %>% select(-c(uspc_sub, year))
event_study_df_2 = bind_cols(nafill(event_study_df_2, fill = 0))
event_study_df = bind_cols(event_study_df_1[, .(uspc_sub, year)], event_study_df_2)

event_study_df[, treated := ifelse(uspc_sub %in% nasa_classes$uspc_sub_long, 1, 0)]
event_study_df[, event_time := ifelse(treated == 1, year - 1958, 0)]
event_study_df[, D := factor(treated * event_time)]
event_study_df[, D := relevel(D, ref = '-1')]


# CREATE ALTERNATIVE EVENT STUDY DFS -------------------------------------------


# Besides main specification where other government classes are used as control
# create one with all classes and one where I only use narrow classes within
# treated broad classes

# All classes
event_study_all_df_1 = merge(citations_df, counts_df, 
                           by = c('uspc_sub', 'year'), all = TRUE)
event_study_all_df_1 = event_study_all_df_1[year %in% 1948:1980 &
                                          uspc_sub %in% first_year[first_year <= 1958]$uspc_sub]
event_study_all_df_2 = event_study_all_df_1 %>% select(-c(uspc_sub, year))
event_study_all_df_2 = bind_cols(nafill(event_study_all_df_2, fill = 0))
event_study_all_df = bind_cols(event_study_all_df_1[, .(uspc_sub, year)], event_study_all_df_2)

event_study_all_df[, treated := ifelse(uspc_sub %in% nasa_classes$uspc_sub_long, 1, 0)]
event_study_all_df[, event_time := ifelse(treated == 1, year - 1958, 0)]
event_study_all_df[, D := factor(treated * event_time)]
event_study_all_df[, D := relevel(D, ref = '-1')]

# Within treated broad classes
uspc_codes = unique(all_patents[, .(uspc, uspc_sub_long)])
setnames(uspc_codes, 'uspc_sub_long', 'uspc_sub')
nasa_classes_2 = merge(nasa_classes %>% rename(uspc_sub = uspc_sub_long), 
                       uspc_codes, 
                       by = 'uspc_sub', all.x = TRUE)
event_study_within_df = merge(event_study_all_df, uspc_codes, by = 'uspc_sub', all.x = TRUE)
event_study_within_df = event_study_within_df[uspc %in% nasa_classes_2$uspc]


# CREATE PATENT LEVEL DATASET --------------------------------------------------


# Add a NASA dummy to the all_patents dataset and filter to post 1958 to estimate
# lifetime citations difference in means
diff_in_means_df = all_patents[year_issue >= 1958, .(patent_id, uspc_sub_long, year_issue)]
setnames(diff_in_means_df, 'uspc_sub_long', 'uspc_sub')
diff_in_means_df[, nasa := ifelse(patent_id %in% nasa_patent_ids$patent_id, 1, 0)]
diff_in_means_citations = citations[cited %in% diff_in_means_df$patent_id,
                                    .(citing, cited)]
diff_in_means_citations = diff_in_means_citations[, 
                                                  .(lifetime_citations = .N),
                                                  by = .(cited)]
setnames(diff_in_means_citations, 'cited', 'patent_id')
diff_in_means_df = merge(diff_in_means_df, diff_in_means_citations,
                         by = 'patent_id', all.x = TRUE)
diff_in_means_df[, lifetime_citations := ifelse(is.na(lifetime_citations), 
                                                0, lifetime_citations)]


# MISCELLANEOUS DATASETS -------------------------------------------------------


# Count how many classes did NASA hold the first patent for (NASA seeded classes)
all_patents[, disp_dt_parsed := dmy(disp_dt)]
first_patent = all_patents[, .(disp_dt_parsed = min(disp_dt_parsed, na.rm = TRUE)),
                           by = uspc_sub_long]
first_patent = first_patent[!is.na(disp_dt_parsed)]
first_patent = merge(first_patent, 
                     all_patents[, .(patent_id, disp_dt_parsed, uspc_sub_long)],
                     by = c('disp_dt_parsed', 'uspc_sub_long'),
                     all = FALSE)
first_patent[, nasa := ifelse(patent_id %in% govt_patents[nasa > 0]$patent_id, 1, 0)]


# Sample of patents in treated subclasses that are not NASA
# Handcheck to see if the Fleming data is undercounting NASA patents
treated_classes = unique(event_study_df[treated == 1, .(uspc_sub)])
treated_non_nasa_patents = all_patents[uspc_sub_long %in% treated_classes$uspc_sub & 
                                         year_issue %in% 1958:1980 & 
                                         !(patent_id %in% fleming_data[nasa > 0])]
nrow(treated_non_nasa_patents)
set.seed(60637)
treated_non_nasa_patents_sample = treated_non_nasa_patents[sample(.N, 500)]


# SAVE DATASETS ----------------------------------------------------------------


# Save baseline event study dataframe
fwrite(event_study_df, 'NASA/final_output/event_study_df.csv')

# Save all class event study dataframe
fwrite(event_study_all_df, 'NASA/final_output/event_study_all_class_df.csv')

# Save within treated broad class event study dataframe
fwrite(event_study_within_df, 'NASA/final_output/event_study_within_treated_class_df.csv')

# Save a dataset for the difference in means
fwrite(diff_in_means_df, 'NASA/final_output/diff_in_means_df.csv')

# Save a dataset with each technology subclass' first patent
fwrite(first_patent, 'NASA/final_output/first_patent_each_class.csv')

# Save a dataset with a random sample of non-NASA in treated subclasses
fwrite(treated_non_nasa_patents_sample, 'NASA/final_output/treated_non_nasa_patents_sample.csv')
