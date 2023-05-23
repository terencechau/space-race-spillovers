# This script takes the Fleming et al. (2019) data and the USPTO HPDF (Marco et al, 2015)
# data to construct a technology subclass by year panel dataset for the main analysis in
# Public R&D and Spillovers: Evidence from NASA Patents (Chau, 2022)

import pandas as pd
import numpy as np
import os
import gc

os.chdir("/Users/terencechau/Dropbox/Work/Research")

# Load Fleming data, which contains federal reliance attribution for each patent
# Metadata contains specifies the federal agency involved
# Category details the type of federal reliance
# Note: R version loads the original tsvs using data.table, but Python can't parse a
# typo with an extra quotation mark in the data, so the files have been converted to csv
# and engine set to "python" to work here
fleming_data = pd.read_csv(
    "NASA/input/government_patents/uspto.govt.reliance.metadata.csv", engine="python"
)
fleming_data_2 = pd.read_csv(
    "NASA/input/government_patents/uspto.govt.reliance.category.csv", engine="python"
)
fleming_data.columns = fleming_data.columns.str.lower()
fleming_data_2.columns = fleming_data_2.columns.str.lower()

# Save patents that are directly owned or contracted by a federal agency
category_1_2 = fleming_data_2.query(
    "category_1_owned_by_usgovt==1|category_2_acknowledge_usgovt_support==1"
)["patno"]

fleming_data = fleming_data.query("patno in @category_1_2")
del category_1_2

# Load HPDF data, which contains all patents issued and their technology classification
all_patents = pd.read_csv(
    "Patents/input/USPTO HPDF/historical_masterfile.csv",
    dtype={"uspc": str, "uspc_sub": str, "disp_dt": str},
)

# Drop patents with non-valid USPC codes
invalid_uspcs = ["", "000", "PLT", "XXX", "999"]
all_patents = all_patents.loc[np.logical_not(all_patents["uspc"].isin(invalid_uspcs))]
all_patents = all_patents.dropna(subset="uspc")

# Drop design patents
all_patents = all_patents.loc[np.logical_not(all_patents["uspc"].str.contains("D"))]

# Drop patents with non-valid USPC subclass codes
invalid_uspc_subs = ["", "PCT", "XXX"]
all_patents = all_patents.loc[
    np.logical_not(all_patents["uspc_sub"].isin(invalid_uspc_subs))
]
all_patents = all_patents.dropna(subset="uspc_sub")

all_patents = all_patents.rename(columns={"patent": "patent_id"})
fleming_data = fleming_data.rename(columns={"patno": "patent_id"})

all_patents["uspc"] = all_patents["uspc"].map(str).str.replace("\.[0-9]*", "")
all_patents["uspc_sub"] = all_patents["uspc_sub"].map(str)
all_patents["uspc_sub_long"] = all_patents["uspc"].str.cat(
    all_patents["uspc_sub"], sep="_"
)
all_patents["year_issue"] = all_patents["disp_dt"].str[5:]
all_patents["year_issue"] = all_patents["year_issue"].astype("Int64")

# Add technology class to government patents
govt_patents = pd.merge(
    fleming_data[
        ["patent_id", "grantyear", "dod", "doe", "nasa", "nsf", "hhs", "usda", "others"]
    ],
    all_patents[["patent_id", "uspc", "uspc_sub_long"]],
    how="left",
    on="patent_id",
)

#################################################################################
############################# GET CITATIONS #####################################
#################################################################################

# Prepare citation data

# Load citation data from Fleming
old_citations = pd.read_table(
    "NASA/input/government_patents/uspto.citation.1926-1975.tsv"
)
new_citations = pd.read_table(
    "NASA/input/government_patents/uspto.citation.1976-2017.tsv"
)

# Filter to citations made to US patents
new_citations = new_citations.query("country in ['US', 'USA']")

# Clean and count forward citations for each patent in the Fleming data
old_citations = old_citations.drop(columns="id")
new_citations = new_citations.drop(columns="country")

citations = pd.concat([old_citations, new_citations])

del old_citations
del new_citations
gc.collect()

citations["cited"] = citations["cited"].astype("string")
citations["citing"] = citations["citing"].astype("string")
citations = citations[citations["cited"].str.contains("^[0-9]+$")]
citations = citations[citations["citing"].str.contains("^[0-9]+$")]

# Identify if citing or cited patents are NASA
nasa_patent_ids = fleming_data.query("nasa > 0")["patent_id"]

citations["citing_nasa"] = np.where(citations["citing"].isin(nasa_patent_ids), 1, 0)
citations["cited_nasa"] = np.where(citations["cited"].isin(nasa_patent_ids), 1, 0)

# Merge cited patent year of issue and uspc into citations data from HPDF file
all_patents = all_patents.rename(
    columns={
        "patent_id": "cited",
        "year_issue": "cited_year",
        "uspc": "cited_uspc",
        "uspc_sub_long": "cited_uspc_sub",
    }
)
citations = pd.merge(
    citations,
    all_patents[["cited", "cited_year", "cited_uspc", "cited_uspc_sub"]],
    how="left",
    on="cited",
)

# Merge citing patent year of issue and uspc into citations data from HPDF file
all_patents = all_patents.rename(
    columns={
        "cited": "citing",
        "cited_year": "citing_year",
        "cited_uspc": "citing_uspc",
        "cited_uspc_sub": "citing_uspc_sub",
    }
)
citations = pd.merge(
    citations,
    all_patents[["citing", "citing_year", "citing_uspc", "citing_uspc_sub"]],
    how="left",
    on="citing",
)

citations["citation_delta"] = citations["citing_year"] - citations["cited_year"]

# Reset names in all patents
all_patents = all_patents.rename(
    columns={
        "citing": "patent_id",
        "citing_year": "year_issue",
        "citing_uspc": "uspc",
        "citing_uspc_sub": "uspc_sub_long",
    }
)

# Count citations at the subclass by year level
# For each citation outcome, count: total, total excluding NASA, leave one out
# subclass (LOO), leave one out excluding NASA, leave one out broad class (LOO B),
# leave one out broad excluding NASA.
# Also count number of other subclasses citing reference class
citations["cond_1"] = np.where(
    np.logical_or(citations["cited_nasa"] != 1, citations["citing_nasa"] != 1),
    1,
    0,
)

citations["cond_2"] = np.where(
    np.logical_not(citations["cited_uspc_sub"] == citations["citing_uspc_sub"]),
    1,
    0,
)

citations["cond_3"] = np.where(
    np.logical_not(citations["cited_uspc"] == citations["citing_uspc"]), 1, 0
)

citations["cond_4"] = np.where(
    np.logical_not(citations["citation_delta"].isin(np.arange(0, 20))), 1, 0
)

citations["cond_5"] = np.where(
    np.logical_and(citations["cond_1"], citations["cond_2"]), 1, 0
)

citations["cond_6"] = np.where(
    np.logical_and(citations["cond_1"], citations["cond_3"]), 1, 0
)

citations["cond_7"] = np.where(
    np.logical_and(citations["cond_1"], citations["cond_4"]), 1, 0
)

citations["cond_8"] = np.where(
    np.logical_and(citations["cond_2"], citations["cond_4"]), 1, 0
)

citations["cond_9"] = np.where(
    np.logical_and(citations["cond_3"], citations["cond_4"]), 1, 0
)

citations["cond_10"] = np.where(
    np.logical_and(citations["cond_5"], citations["cond_4"]),
    1,
    0,
)

citations["cond_11"] = np.where(
    np.logical_and(citations["cond_6"], citations["cond_4"]),
    1,
    0,
)

# Get lifetime and 20 year window citations
citations_df_1 = citations.groupby(
    ["cited_uspc_sub", "cited_year"], as_index=False
).agg(
    citations_lifetime=("citing", "count"),
    citations_lifetime_excl_nasa=("cond_1", "sum"),
    citations_lifetime_loo=("cond_2", "sum"),
    citations_lifetime_loo_excl_nasa=("cond_5", "sum"),
    citations_lifetime_loo_b=("cond_3", "sum"),
    citations_lifetime_loo_b_excl_nasa=("cond_6", "sum"),
    citations_20_yr=("cond_4", "sum"),
    citations_20_yr_excl_nasa=("cond_7", "sum"),
    citations_20_yr_loo=("cond_8", "sum"),
    citations_20_yr_loo_excl_nasa=("cond_10", "sum"),
    citations_20_yr_loo_b=("cond_9", "sum"),
    citations_20_yr_loo_b_excl_nasa=("cond_11", "sum"),
)

# Get citations by year
citations_df_2 = citations.groupby(
    ["cited_uspc_sub", "citing_year"], as_index=False
).agg(
    citations_year=("citing", "count"),
    citations_year_excl_nasa=("cond_1", "sum"),
    citations_year_loo=("cond_2", "sum"),
    citations_year_loo_excl_nasa=("cond_5", "sum"),
    citations_year_loo_b=("cond_3", "sum"),
    citations_year_loo_b_excl_nasa=("cond_6", "sum"),
)

# Fix names and merge into one df
# Note from now on, uspc_sub refers to the cited uspc_sub
citations_df_1 = citations_df_1.rename(columns={"cited_year": "year"})
citations_df_2 = citations_df_2.rename(columns={"citing_year": "year"})

citations_df = pd.merge(
    citations_df_1, citations_df_2, on=["cited_uspc_sub", "year"], how="outer"
)

del citations
del citations_df_1
del citations_df_2
gc.collect()

citations_df = citations_df.dropna(subset=["cited_uspc_sub", "year"], how="any")
citations_df = citations_df[
    np.logical_and(citations_df["year"] != "", citations_df["cited_uspc_sub"] != "")
]

citations_df = citations_df.rename(columns={"cited_uspc_sub": "uspc_sub"})
citations_df = citations_df.sort_values(by=["uspc_sub", "year"])

#################################################################################
############################## GET COUNTS #######################################
#################################################################################

# Counts by issue year
counts_issued = all_patents.groupby(
    ["uspc_sub_long", "year_issue"], as_index=False
).agg(issued=("patent_id", "count"))
counts_issued = counts_issued.rename(columns={"year_issue": "year"})
counts_issued = counts_issued.dropna(subset=["uspc_sub_long", "year"], how="any")
counts_issued = counts_issued.sort_values(by=["uspc_sub_long", "year"])

# Counts by issue year without NASA
all_patents["not_nasa"] = np.where(all_patents["patent_id"].isin(nasa_patent_ids), 0, 1)
counts_issued_excl_nasa = all_patents.groupby(
    ["uspc_sub_long", "year_issue"], as_index=False
).agg(issued_excl_nasa=("not_nasa", "sum"))
counts_issued_excl_nasa = counts_issued_excl_nasa.rename(columns={"year_issue": "year"})
counts_issued_excl_nasa = counts_issued_excl_nasa.dropna(
    subset=["uspc_sub_long", "year"]
)
counts_issued_excl_nasa = counts_issued_excl_nasa.sort_values(
    by=["uspc_sub_long", "year"]
)

# Counts by application year (using Enrico's data)
cusp_years = pd.read_csv("CUSP Berkes Data/patents_fyear_iyear_1940_1980.csv")
cusp_years = cusp_years.rename(columns={"patnum": "patent_id", "fyear": "year"})
cusp_years["patent_id"] = cusp_years["patent_id"].astype("string")
cusp_years["year"] = cusp_years["year"].astype("Int64")
cusp_years = pd.merge(
    cusp_years, all_patents[["patent_id", "uspc_sub_long"]], on="patent_id", how="left"
)
counts_applied = cusp_years.groupby(["uspc_sub_long", "year"], as_index=False).agg(
    applied=("patent_id", "count")
)

counts_df = pd.merge(
    counts_issued, counts_issued_excl_nasa, on=["uspc_sub_long", "year"], how="outer"
)
counts_df = pd.merge(
    counts_df, counts_applied, on=["uspc_sub_long", "year"], how="outer"
)
counts_df = counts_df.rename(columns={"uspc_sub_long": "uspc_sub"})
counts_df = counts_df.dropna(subset="uspc_sub")
counts_df = counts_df.sort_values(by=["uspc_sub", "year"])

del counts_issued
del counts_issued_excl_nasa
del counts_applied
del cusp_years
gc.collect()

#################################################################################
##################### GET FIRST YEAR FOR EACH USPC_SUB ##########################
#################################################################################

first_year = all_patents.groupby("uspc_sub_long", as_index=False).agg(
    first_year=("year_issue", "min")
)
first_year = first_year.rename(columns={"uspc_sub_long": "uspc_sub"})

#################################################################################
######################### CREATE EVENT STUDY DF #################################
#################################################################################

# Assemble control group and DOD/NASA overlaps
agency_list = ["dod", "nasa", "doe", "nsf", "hhs", "usda", "others"]
govt_patents[agency_list] = (govt_patents[agency_list] > 0).astype("Int64")
govt_classes = govt_patents.groupby(by="uspc_sub_long").sum(numeric_only=True)[
    agency_list
]
govt_classes = govt_classes.loc[(govt_classes != 0).any(axis=1)]
govt_classes = govt_classes.reset_index(level=0)

govt_classes_pre = (
    govt_patents.query("grantyear >= 1948 and grantyear <= 1980")
    .groupby(by="uspc_sub_long")
    .sum(numeric_only=True)[agency_list]
)
govt_classes_pre = govt_classes_pre.loc[(govt_classes_pre != 0).any(axis=1)]
govt_classes_pre = govt_classes_pre.reset_index(level=0)

nasa_classes = govt_classes.loc[govt_classes["nasa"] > 0]
nasa_dod_overlap = nasa_classes.loc[nasa_classes["dod"] > 0]

# Prepare regression sample:
# 1. Subset all data from 1948 to 1980
# 2. Narrow technology classes that were present before 1958
# 3. Classes that some government agency worked on before 1958 (control) and NASA classes
# worked on 1958 to 1972 (treatment).
# 4. Classes that do already exist get NA filled with zero but only after their existence year
# (because the class is confirmed to exist, but not all years will get a patent)

event_study_df = pd.merge(citations_df, counts_df, on=["uspc_sub", "year"], how="outer")
event_study_df = event_study_df.loc[
    np.logical_and(event_study_df["year"] >= 1948, event_study_df["year"] <= 1980)
]

pre_1958_uspcs = first_year.loc[first_year["first_year"] <= 1958, "uspc_sub"]
event_study_df = event_study_df.loc[event_study_df["uspc_sub"].isin(pre_1958_uspcs)]
event_study_df = event_study_df.loc[
    event_study_df["uspc_sub"].isin(
        np.logical_or(govt_classes_pre["uspc_sub_long"], nasa_classes["uspc_sub_long"])
    )
]
event_study_df = event_study_df.fillna(0)

event_study_df["treated"] = (
    event_study_df["uspc_sub"].isin(nasa_classes["uspc_sub_long"]).astype("int")
)
event_study_df["event_time"] = np.where(
    event_study_df["treated"] == 1, event_study_df["year"] - 1958, 0
)
event_study_df["D"] = (event_study_df["treated"] * event_study_df["event_time"]).astype(
    "category"
)

event_study_df.to_csv("NASA/final_output/event_study_df_python.csv")
