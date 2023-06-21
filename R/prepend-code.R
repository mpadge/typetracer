
# Modified from original 'injectr' code by Filip Krikava
# https://github.com/PRL-PRG/injectr

prepend_code <- function (orig_code, code) {
    # is.language will not work since SYMSXP and EXPRSXP are also of language
    # type
    # if (typeof (orig_code) == "language" &&
    #     identical (orig_code [[1]], as.name ("{"))) {
    #     as.call (append (as.list (orig_code), code, 1))
    # }

    # fns do not need curly braces to be defined:
    if (typeof (orig_code) == "language") {
        as.call (append (as.list (orig_code), code, 1))
    }
}
