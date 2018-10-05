
generate_pls <- function(.data) {
    lhr <- .data %>%
        filter(VN == 0) %>%
        select_at(c(fa_conc_vars, outcome_vars)) %>%
        na.omit() %>%
        pls_model(.data = .,
                  .xvar = fa_conc_vars,
                  .yvar = "LegHtRatio")
    height <- .data %>%
        filter(VN == 0) %>%
        select_at(c(fa_conc_vars, outcome_vars)) %>%
        na.omit() %>%
        pls_model(.data = .,
                  .xvar = fa_conc_vars,
                  .yvar = "Height")
    pls_results <- list(conc = list(lhr = lhr, height = height))
    devtools::use_data(pls_results, overwrite = TRUE)
}
