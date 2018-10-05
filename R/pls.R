
pls_model <- function(.data, .xvar, .yvar, ncomp = NULL) {
    .data %>%
        design("pls") %>%
        add_settings(ncomp = ncomp,
                     validation = "CV",
                     cv.data = FALSE) %>%
        add_variables("xvar", .xvar) %>%
        add_variables("yvar", .yvar) %>%
        construct() %>%
        scrub()
}

loadings_as_df <- function(model) {
    loading_nums <- unclass(pls::loadings(model))

    loading_nums %>%
        as_data_frame() %>%
        mutate(xvariables = dimnames(loading_nums)[[1]])

}

append_large_loadings <- function(.loadings, load_range = c(-0.25, 0.25)) {
    .loadings %>%
        split_fractions(xvariables) %>%
        tidyr::gather(components, loadings, -xvariables, -Fractions) %>%
        order_fractions(Fractions) %>%
        mutate(loadings = as.numeric(loadings)) %>%
        group_by(components) %>%
        mutate(
            max_loading = max(loadings),
            min_loading = min(loadings),
            large_loadings = !between(loadings, load_range[1], load_range[2]),
            xvariables = ifelse(large_loadings, xvariables, NA)
        ) %>%
        ungroup()
}

scores_as_df <- function(model) {
    .scores <- pls::scores(model)
    attr(.scores, "explvar") <- NULL
    .scores <- .scores %>%
        unclass() %>%
        as.matrix() %>%
        as_tibble() %>%
        rename_all(funs(stringr::str_replace(., " ", "")))

    .data <- bind_cols(
        model$model$X %>%
            as_tibble(),
        model$model$Y %>%
            as_tibble()
        )

    bind_cols(.data, .scores)
}

calc_radius <- function(x, y) {
    sqrt(x ^ 2 + y ^ 2)
}

large_contributors <- function(.model) {
    expl_var_50 <- sqrt(1 / 2)
    .model %>%
        model.matrix() %>%
        cor(pls::scores(.model)[, 1:2, drop = FALSE]) %>%
        as_tibble(rownames = "fattyacid") %>%
        rename_all(funs(stringr::str_replace(., " ", ""))) %>%
        mutate(fattyacid = PROMISE.misc::renaming_fa(fattyacid) %>%
                   stringr::str_replace("pct_", "")) %>%
        filter(calc_radius(Comp1, Comp2) >= expl_var_50)

}
