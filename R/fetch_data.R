#' Fetch data from the original source
#'
#' This function fetchs the main dataset, keeps variables relevant to
#' the analysis, restrict the sample size as needed, and lastly save
#' the new dataset as an `.RData` file.
#'
#' @return Saves the wrangled data into the data/ folder.
#'
fetch_data <- function() {
    # Load the master dataset,
    project_data <- PROMISE.data::PROMISE %>%
        filter(DM != 1 | is.na(DM)) %>%
        filter(VN %in% c(0, 3, 6)) %>%
        select(SID, VN, YearsFromBaseline, SitHeight, Height, BMI, Glucose0,
               Sex, Ethnicity, Age,
               TAG, Chol, LDL, HDL, ALT, CRP, Waist, MAP = MeanArtPressure) %>%
        mutate(BaselineAge = if_else(VN == 0, Age, NA_real_)) %>%
        group_by(SID) %>%
        arrange(SitHeight) %>%
        tidyr::fill(SitHeight) %>%
        arrange(VN) %>%
        tidyr::fill(BaselineAge) %>%
        ungroup() %>%
        arrange(SID, VN) %>%
        mutate(SitHeight = SitHeight - 52.6,
               SitHtRatio = SitHeight / Height,
               LegHtRatio = (Height - SitHeight) / Height,
               invHDL = 1 / HDL)

    fattyacids <- PROMISE.data::fattyacids %>%
        select(SID, VN, matches("^(ne|pl|ce|tg)")) %>%
        mutate(TotalFA = rowSums(select(., matches("^(ne|tg|pl|ce)"))))

    fattyacids <- fattyacids %>%
        select_at(vars(matches("^(ne|tg|pl|ce)"))) %>%
        rename_all(funs(paste0("pct_", .))) %>%
        mutate_all(funs((. / fattyacids$TotalFA) * 100)) %>%
        bind_cols(fattyacids)

    project_data <- project_data %>%
        select_at(vars(TAG, ALT, CRP)) %>%
        rename_all(funs(paste0("l", .))) %>%
        mutate_all(log) %>%
        bind_cols(project_data) %>%
        full_join(fattyacids)

    project_data <- project_data %>%
        filter(SitHtRatio < 0.65)

    # Save the dataset to the data/ folder.
    devtools::use_data(project_data, overwrite = TRUE)
}
