
#' Create two new columns, one with fraction, the other with properly formatted fatty acids.
#'
#' @param .data Project data.
#' @param variable Column with fatty acid variable names.
#'
split_fractions <- function(.data, variable) {
    fattyacids <- enquo(variable)
    fa_name <- quo_name(fattyacids)
    .data %>%
        mutate(
            Fractions = PROMISE.misc:::extract_fa_fraction(!!fattyacids, TRUE),
            !!fa_name := renaming_fats(!!fattyacids)
        )
}

#' Alphabetically set fractions.
#'
#' @param .data Project data.
#' @param fraction_column Fraction column.
#'
order_fractions <- function(.data, fraction_column) {
    fractions <- enquo(fraction_column)
    .data %>%
        arrange(!! fractions)
}

#' Order fatty acid variables (in one column) by "0", then n-9, n-7, etc.
#'
#' @param .data Project data
#' @param fattyacid_column Column with fatty acid variables
#'
order_fatty_acids <- function(.data, fattyacid_column) {
    extract_ordering <- function(x) {
        ord <- stringr::str_sub(x, -1)
        case_when(
            ord == "0" ~ 10L,
            TRUE ~ as.integer(ord)
        )
    }
    fattyacids <- enquo(fattyacid_column)
    fa_name <- quo_name(fattyacids)
    .data %>%
        mutate(ordering = extract_ordering(!! fattyacids)) %>%
        arrange(desc(ordering)) %>%
        select(-ordering) %>%
        mutate(!! fa_name := forcats::fct_inorder(!! fattyacids))
}

#' Rename fatty acids to appropriate format (20:0, 18:3 n-3, etc).
#'
#' @param x fatty acids vector, as when doing mutate.
#'
renaming_fats <- function(x) {
    x %>%
        stringr::str_remove('^pct_') %>%
        PROMISE.misc::renaming_fa(keep.fraction = FALSE)
}
