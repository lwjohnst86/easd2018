
.onLoad <- function(libname, pkgname) {
    if (!require(PROMISE.data, quietly = TRUE)) {
        message(". The PROMISE.data package is not installed. It is not necessary ",
                "to install it, but the project_data can not be updated.")
    }

    invisible()
}
