# This script replicates the data cleaning in the main cleaning data script for
# Public R&D and Spillovers: Evidence from NASA Patents (Chau, 2023), located in
# R_replication_package/1_prepare_data.R using SQL queries instead. Queries are
# written as strings then passed to `sqldf::sqldf()` for interpreting.

library(sqldf)

setwd('~/Dropbox/Work/Research')
source('NASA/final_code/R_replication_package/functions.R')

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
query = 'SELECT *
FROM fleming_data
WHERE patno IN (SELECT patno
                FROM fleming_data_2
                WHERE category_1_owned_by_usgovt = 1 
                OR category_2_acknowledge_usgovt_support = 1)
;'
fleming_data = sqldf(query)

# Load HPDF data, which contains all utility patents issued and their technology classification
all_patents = fread('~/Dropbox/Work/Research/Patents/input/USPTO HPDF/historical_masterfile.csv')

# Drop patents with non-valid USPC codes
query = 'SELECT *
FROM all_patents
WHERE uspc NOT IN ("", "000", "PLT", "XXX", "999") 
;'
all_patents = sqldf(query)

# Drop design patents
query = 'SELECT *
FROM all_patents
WHERE uspc NOT LIKE "%D%"'
all_patents = sqldf(query)

# Drop patents with non-valid USPC subclass codes
query = 'SELECT *
FROM all_patents
WHERE uspc_sub NOT IN ("", "PCT", "XXX")
;'
all_patents = sqldf(query)

# Note: sqldf doesn't allow ALTER statements but SQL code included here
query = 'ALTER TABLE fleming_data
RENAME COLUMN patno TO patent_id
;'
query = 'ALTER TABLE all_patents
RENAME COLUMN patent TO patent_id'

setnames(fleming_data, 'patno', 'patent_id')
setnames(all_patents, 'patent', 'patent_id')

# sqldf uses SQLite by default, so || instead of CONCAT()
query = 'SELECT *,
uspc|| "_" ||uspc_sub AS uspc_sub_long,
SUBSTRING(disp_dt, 6) AS year_issue
FROM all_patents
;'
all_patents = sqldf(query)

# Add technology class to government patents
query = 'SELECT 
f.patent_id, 
f.grantyear, 
f.dod, 
f.doe, 
f.nasa, 
f.nsf, 
f.hhs, 
f.usda, 
f.others, 
a.uspc, 
a.uspc_sub_long
FROM fleming_data AS f
LEFT JOIN all_patents AS a
  ON f.patent_id = a.patent_id
;'
govt_patents = sqldf(query)

# GET CITATIONS-----------------------------------------------------------------


# Prepare citation data

# Load citation data from Fleming
old_citations = fread('NASA/input/government_patents/uspto.citation.1926-1975.tsv')
new_citations = fread('NASA/input/government_patents/uspto.citation.1976-2017.tsv')

# Filter to citations made to US patents
query = 'SELECT *
FROM new_citations
WHERE country IN ("US", "USA")'
new_citations = sqldf(query)

# Clean and count forward citations for each patent in the Fleming data
query = 'SELECT CAST(citing AS text) AS citing, CAST(cited AS text) AS cited
FROM old_citations
UNION
SELECT CAST(citing AS text) AS citing, CAST(cited AS text) AS cited
FROM new_citations'
citations = sqldf(query)

citations = citations[str_detect(cited, pattern = '^[:digit:]+$')]
citations = citations[str_detect(citing, pattern = '^[:digit:]+$')]

# Identify if citing or cited patents are NASA
citations[, citing_nasa := ifelse(citing %in% fleming_data[nasa > 0]$patent_id, 1, 0)]
citations[, cited_nasa := ifelse(cited %in% fleming_data[nasa > 0]$patent_id, 1, 0)]










