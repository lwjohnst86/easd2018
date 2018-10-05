
# PLS ---------------------------------------------------------------------


#' Create a plot of the PLS loadings of each variable in the component.
#'
#' @param .model PLS model
#' @param load_range range between which all variables are greyed out
#' @param ncomps number of components
#'
plot_loadings <- function(.model, load_range = c(-0.25, 0.25), ncomps = 1:2) {
    total_expl_var <- pls::explvar(.model)[ncomps] %>%
        sum() %>%
        round(0) %>%
        as.character()

    plot_data <- .model %>%
        loadings_as_df() %>%
        append_large_loadings(load_range) %>%
        filter(components %in% paste0("Comp ", ncomps)) %>%
        mutate(components = stringr::str_replace(components, "omp ", "")) %>%
        mutate(components = forcats::fct_rev(components),
               Fractions = forcats::as_factor(Fractions))

    plot_data %>%
        ggplot(aes(x = loadings, y = components, colour = Fractions)) +
        geom_point(aes(alpha = large_loadings)) +
        ggrepel::geom_text_repel(
            aes(label = xvariables),
            size = 2.5,
            nudge_y = 0.15,
            direction = "x",
            angle = 90,
            vjust = 0,
            box.padding = 0.05,
            segment.size = 0.2,
            na.rm = TRUE
        ) +
        coord_cartesian(ylim = c(0.85, 2.6), xlim = c(-0.35, 0.35), expand = FALSE) +
        scale_colour_manual(values = fraction_palette,
                            na.translate = TRUE,
                            na.value = "grey40",
                            breaks = unique(na.omit(plot_data$Fractions))) +
        labs(y = paste0("Components (", total_expl_var, "% total \nexplained variance)"),
             x = "Component loadings") +
        scale_alpha_discrete(guide = "none") +
        poster_plot_theme

}

plot_scores <- function(.model) {
    yvar <- dimnames(.model$model$Y)[[2]]
    .scores <- .model %>%
        scores_as_df()
    comp_vars <- grep("^Comp", names(.scores), value = TRUE)

    .output <- .scores %>%
        select_at(c(yvar, comp_vars)) %>%
        tidyr::gather_("Component", "Score", comp_vars) %>%
        filter(Component %in% paste0("Comp", 1:6)) %>%
        tidyr::gather_("Yvariable", "Value", yvar) %>%
        mutate_at("Component", funs(stringr::str_replace(., "Comp", "Component ")))

    .output %>%
        ggplot(aes(x = Score, y = Value)) +
        geom_point() +
        facet_wrap(Component ~ Yvariable, scales = "free")
}


#' Create a plot of the correlation circle results by two components.
#'
#' @param .model PLS model
#' @param title title for plot
#'
plot_corr_comps <- function(.model, title = NULL) {
    expl_var_50 <- sqrt(1 / 2)

    fit <- cor(model.matrix(.model), pls::scores(.model)[, 1:2, drop = FALSE]) %>%
        as_tibble(rownames = "xvariables") %>%
        setNames(c('xvariables', 'C1', 'C2')) %>%
        split_fractions(xvariables) %>%
        mutate(xvariables = ifelse(calc_radius(C1, C2) >= expl_var_50, xvariables, NA)) %>%
        mutate(large_loadings = ifelse(calc_radius(C1, C2) >= expl_var_50, TRUE, FALSE))

    circle_outer <- seer:::.circle_data(1)
    circle_inner <- seer:::.circle_data(sqrt(1 / 2))

    fig <- ggplot(fit, aes_string(x = "C1", y = "C2")) +
        geom_segment(aes(
            x = -1,
            y = 0,
            xend = 1,
            yend = 0
        ), colour = 'grey90') +
        geom_segment(aes(
            x = 0,
            y = -1,
            xend = 0,
            yend = 1
        ), colour = 'grey90') +
        geom_path(data = circle_outer, aes(x = x, y = y), colour = "grey70") +
        geom_path(data = circle_inner, aes(x = x, y = y), linetype = 'dotted', colour = "grey50") +
        geom_point(data = fit, aes(alpha = large_loadings, colour = Fractions)) +
        scale_alpha_discrete(guide = "none") +
        ggrepel::geom_text_repel(
            data = fit,
            aes(label = xvariables, colour = Fractions),
            size = 2.5,
            box.padding = 0.4,
            segment.alpha = 0.3,
            na.rm = TRUE
        ) +
        scale_colour_manual(values = fraction_palette,
                            na.translate = TRUE,
                            na.value = "grey40",
                            breaks = unique(na.omit(fit$Fractions))) +
        labs(
            x = paste0('C1 (', round(pls::explvar(.model)[1], 1), '% explained variance)'),
            y = paste0('C2 (', round(pls::explvar(.model)[2], 1), '% explained variance)'),
            title = title
        ) +
        poster_plot_theme
    fig
}


# Distributions -----------------------------------------------------------

#' Plots the distribution of the TAGFA composition.
#'
#' @param data Project data.
#'
plot_fattyacids <- function(.data) {
    only_fatty_acids <- .data %>%
        filter(VN == 0) %>%
        select_at(fa_conc_vars) %>%
        tidyr::gather(FattyAcids, Concentration) %>%
        stats::na.omit()

    ordered_by_fatty_acid_type <- only_fatty_acids %>%
        split_fractions(FattyAcids) %>%
        order_fatty_acids(FattyAcids)

    ordered_by_fatty_acid_type %>%
        ggplot(aes(y = Concentration, x = FattyAcids, colour = Fractions)) +
        geom_boxplot(outlier.shape = NA) +
        coord_flip() +
        labs(y = 'Concentration (nmol/mL)',
                      x = 'Fatty acids') +
        scale_y_continuous(breaks = scales::pretty_breaks(3)) +
        scale_colour_manual(values = fraction_palette) +
        poster_plot_theme +
        theme(strip.background = element_blank(),
              legend.position = "none")
}


# Theme -------------------------------------------------------------------

poster_plot_theme <- theme_classic() +
    theme(
        text = element_text(colour = "grey40"),
        title = element_text(colour = "grey40"),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "grey40"),
        plot.title = element_text(size = 12)
        # strip.text = element_text(colour = "grey40"),
    )

remove_legend <- theme(legend.position = "none")

fraction_palette <- RColorBrewer::brewer.pal(11, "Spectral")[c(1, 3, 9, 11)]
