
# Modified from 'injectr' by Filip Krikava
# https://github.com/PRL-PRG/injectr

prepend_code <- function (orig_code, code, use_primitive) {
    # is.language will not work since SYMSXP and EXPRSXP are also of language
    # type
    if (typeof (orig_code) == "language" &&
            identical (orig_code[[1]], as.name ("{")) &&
            !use_primitive) {
        as.call (append (as.list (orig_code), code, 1))
    } else if (use_primitive) {
        substitute (.Primitive ("{") (CODE, ORIG_CODE),
                    list (CODE = code, ORIG_CODE = orig_code))
    } else {
        substitute ({ CODE; ORIG_CODE },                        # nolint
                    list (CODE = code, ORIG_CODE = orig_code))
    }
}
