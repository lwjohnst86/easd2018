
generate_poster <- function() {
    rmarkdown::render("doc/poster.Rmd")
    postr::render_poster_image("doc/poster.html",
                               aspect_ratio = 1 / sqrt(2),
                               poster_width = 841,
                               width = 1800)
    #system("convert poster.png -quality 0 poster.jp2")
    # uses pdf 1.3 dimenions around 68 in by 48 in
    #system("img2pdf poster.jp2 -o poster.pdf")
    # uses pdf 1.4 dimensions around 6.5in by 4.6in
    system("convert doc/poster.png -quality 100 -density 300 doc/poster.pdf")
    # both commands make pdfs that I can't distinguish between for quality
}
