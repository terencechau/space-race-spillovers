# space-race-spillovers
This repository serves as a replication package for Spillovers from Public R&D: Evidence from Space Race Patents. Following is a description of every script included:

* `functions.R`: Is a script loaded at the start of every subsequent file. Defines aesthetic parameters such as plot sizes, creates a uniform formatting for `kable` tables, and sets a uniform theme for `ggplot2`, which is used to generate all figures in the article. Defines a function for estimating sup-t confidence error bands, `calculate_sup_t` for event study estimates, which relies on the `suptCriticalValue` package by Ryan Kessler (https://github.com/ryanedmundkessler/suptCriticalValue). To plot all event study estimates, I define multiple functions, `plot_event_study_prep`, which takes an `lfe::felm` regression object that includes sup-t upper and lower bounds, and prepares it for a standard event study plot with an omitted event-time period of -1. `plot_event_study` takes the output of this function and plots the dynamic two-way fixed effects estimates using sensible aesthetics. `plot_event_study_overlaid` does the same, but takes multiple event study objects and overlays them for easier comparison (e.g., for the leave-one-out estimate).

* `1_prepare_data.R`: Takes the Fleming et al. and HPDF datasets to construct technology subclass by year panels for the analyses. The resulting datasets contain all outcomes in the paper for each relevant sample: `event_study_df.csv` for the main sample, `event_study_all_class_df.csv` for the sample including all possible control subclasses, and `event_study_within_treated_class_df.csv` for the sample that only uses subclasses within broad classes that contain at least one treated subclass. Also produces other datasets, like `diff_in_means_df.csv` for the naive comparison of NASA and non-NASA patent citations in in the post-1958 period, and takes random samples of patents for further handchecking.

* `2_summary_statistics.R`: Creates summary statistics tables and other miscellaneous calculations used throughout the paper, including, the citation difference in means of NASA and non-NASA patents, the difference in differences baseline year balance table, tabulates the largest treatment and control subclasses, and calculates how many classes NASA seeded.

* `3_baseline_event_study.R`: Takes the output from `1_prepare_data.R` and estimates the main static and dynamic two-way fixed effects regressions in the paper. Each static regression is saved as a LaTeX table, while every dynamic regression is saved as a `ggplot2` figure.

* `4_space_essential_classes.R`: Defines the space essential classes, then re-estimates the regressions from the previous file. Creates the mission deviation plot for space-essential classes, which relates essential classes to actually treated classes.

* `5_excluding_weaponry.R`: Re-estimates the analyses in `3_baseline_event_study.R` omitting military-related classes.

* `6_alternative_control_groups.R`: Re-estimates the baseline analyses using the two sets of alternative control groups.

* `7_callaway_santanna.R`: Uses the Callaway and Sant’Anna (2021) estimator to estimate all the main regressions in the paper.

* `8_figures_other.R`: Produces all figures in the paper that don’t rely on the subclass panel directly. Makes all figures related to NASA and federal R&D outlays, computer science enrollments, and top NASA USPC classes and subclasses at the patent level.

* `9_documentation.R`: Calculates all summary statistics and makes all figures in the documentation chapter.

Notes: Partial samples of this code in other languages, like Python and SQL queries that replicate the data preparation steps are provided, but are only for illustration and were not used for any of the results in the article.
