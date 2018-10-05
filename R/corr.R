# Functions to run the correlation analysis
#
# Analyze -----------------------------------------------------------------

#' Correlation analysis.
#'
#' @param data Project data
#' @param x The covariates and outcomes
#' @param y The fatty acids.
#'
#' @export
analyze_corr <- function(.data, .x, .y = NULL) {
    .data %>%
        filter(VN == 0) %>%
        design('cor') %>%
        add_settings(method = 'pearson', use = 'complete.obs') %>%
        {
            if (!is.null(.y))
                add_variables(., 'yvars', .y)
            else
                .
        } %>%
        add_variables('xvars', .x) %>%
        construct() %>%
        scrub() %>%
        mutate(
            Vars2 = factor(Vars2, unique(Vars2)),
            Vars1 = factor(Vars1, unique(Vars1)),
            Correlations = round(Correlations, 2)
        )
}

# Plotting ----------------------------------------------------------------

#' Correlation heatmap plot.
#'
#' @param results Correlation results
#'
#' @export
plot_heatmap <- function(.results, .x = "Vars1", .y = "Vars2", text = TRUE) {
    seer::view_heatmap(
            data = .results,
            y = .y,
            x = .x,
            ylab = "",
            number.colours = 5,
            values.text = text,
            values.size = 4)
        # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
}
