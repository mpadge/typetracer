
testthat_is_parallel <- function (pkg_dir) {

    flist <- list.files (pkg_dir, recursive = TRUE, full.names = TRUE)
    desc <- grep ("DESCRIPTION$", flist, value = TRUE)
    if (length (desc) != 1L) {
        return (FALSE)
    }
    desc <- read.dcf (desc)
    field <- "Config/testthat/parallel"
    if (!field %in% colnames (desc)) {
        return (FALSE)
    }
    ret <- as.logical (desc [1L, field])
    ret <- ifelse (is.na (ret), FALSE, ret)

    return (ret)
}

#' Remove testthat "parallel = true" config entry from DESCRIPTION
#'
#' Tests can not be traced in parallel (issue#10), so this line needs to be
#' removed in order to enable tracing.
#' @noRd
rm_testthat_parallel <- function (pkg_dir) {

    message (
        "Tests can not be traced with testthat tests run in parallel; ",
        "parallel testing has been temporarily deactivated."
    )

    flist <- list.files (pkg_dir, recursive = TRUE, full.names = TRUE)
    desc_file <- grep ("DESCRIPTION$", flist, value = TRUE)
    if (length (desc_file) != 1L) {
        return (NULL)
    }
    desc <- brio::read_lines (desc_file)
    field <- "Config/testthat/parallel"
    desc <- desc [-grep (field, desc, fixed = TRUE)]

    brio::write_lines (desc, desc_file)
}
