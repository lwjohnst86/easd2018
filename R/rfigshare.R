
to_figshare <- function() {
    rfigshare::fs_new_article(
        "Leg length, a marker of early childhood conditions, associates with specific clusters of serum fatty acids",
        "Poster presentation at the European Association for the Study of Diabetes in Berlin, Germany from Oct 1-5, 2018.",
        type = "poster",
        tags = c(
            "type 2 diabetes",
            "fatty acids",
            "lipid fractions",
            "leg length",
            "early childhood conditions",
            "cohort",
            "multivariate",
            "diabetes pathogenesis"
        ),
        categories = c("Diseases", "Pathogenesis", "Epidemiology"),
        files = "doc/poster.pdf",
        visibility = "draft"
    )
}
