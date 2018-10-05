# Calculate or extract for inline -----------------------------------------

#' Calculate the percent contribution of each FA of the whole fraction.
#'
#' @param .data Project data.
#'
#' @export
calc_fa_percent <- function(.data) {
    top_fa <- .data %>%
        select(matches('pct_')) %>%
        tidyr::gather(fat, value) %>%
        group_by(fat) %>%
        summarise(pct = mean(value, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(fat = PROMISE.misc::renaming_fa(fat) %>%
                   stringr::str_remove("pct_"),
               c = paste0(fat, ' (', aide::format_round(pct, 1), '%)')) %>%
        arrange(desc(pct))

    return(top_fa)
}

#' Calculate the n and % of total for a given discrete variable (e.g. Sex).
#'
#' @param x The discrete variable data.
#' @param group Which group to calculate n and % for.
#'
#' @export
calc_discr_npct <- function(x, group = c('Female', 'European')) {
    nums <- table(x)
    pct <- (nums[group] / sum(nums)) * 100
    data.frame(
        n = nums[group],
        pct = paste0(round(pct, 1), '%'),
        npct = paste0(nums[group], ' (', paste0(round(pct, 1), '%)'))
    )
}

#' Check to see how height changes over time.
#'
#' @param .data project data
#'
calc_height_over_time <- function(.data) {
    model <- lme4::lmer(Height ~ VN + (1 | SID), data = project_data)
    model <- broom::tidy(model, conf.int = TRUE)
    over_time <- model %>%
        filter(term == "VN") %>%
        select(estimate, conf.low, conf.high) %>%
        round(2)
    glue::glue("{over_time$estimate} ({over_time$conf.low}, {over_time$conf.high} 95% CI)")
}
