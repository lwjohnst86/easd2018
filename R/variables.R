
data("project_data")
data("pls_results")

fa_pct_vars <- grep("^pct_.*", names(project_data), value = TRUE)

fa_conc_vars <- grep("^(ne|tg|pl|ce)\\d{3}", names(project_data), value = TRUE)

blood_vars <- c("lTAG", "HDL", "MAP", "Glucose0", "lALT", "lCRP")

outcome_vars <- c("Height", "SitHeight", "SitHtRatio", "LegHtRatio")

covariates <- c("YearsFromBaseline", "Waist", "Sex", "Ethnicity", "BaselineAge")
