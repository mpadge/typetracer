is_gh_cov <- identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage.yaml")


test_that ("tracer body", {

    body <- body (typetracer_header)
    if (!is_gh_cov) {
        # covr injects other symbols into code on workflow, so snapshot differs
        testthat::expect_snapshot (body)
    }
})

test_that ("injected tracer body", {

    f <- function (x, y) {
        x * x + y * y
    }
    body0 <- body (f)

    inject_tracer (f)
    body1 <- body (f)

    expect_false (identical (body0, body1))
    expect_true (length (body1) > length (body0))

    expect_equal (body1 [[2]], body (typetracer_header))
    expect_true (uninject_tracer (f))
})

test_that ("No traces", {

    clear_traces ()
    expect_message (
        x <- load_traces (),
        "No traces found; first run 'inject_tracer'"
    )
    expect_null (x)
})

test_that ("trace call", {

    f <- function (x, y) {
        x * x + y * y
    }

    clear_traces ()
    inject_tracer (f)

    val <- f (x = 1:2, y = 3:4 + 0.)
    flist <- list.files (tempdir (),
        pattern = "^typetrace\\_",
        full.names = TRUE
    )
    expect_true (length (flist) > 0L)

    x <- load_traces (files = TRUE)
    expect_true (uninject_tracer (f))

    expect_s3_class (x, "tbl_df")
    expect_equal (nrow (x), 2L) # x and y
    expect_equal (ncol (x), 15L)
    expect_identical (
        names (x),
        c (
            "trace_name", "trace_number",
            "fn_name", "fn_call_hash", "call_env",
            "par_name", "is_named", "class", "typeof", "mode",
            "storage_mode", "length", "formal", "uneval", "eval"
        )
    )
})

test_that ("trace call via namespace-qualified reference", {

    # Namespace-qualified calls ('pkg::fn(...)' / 'pkg:::fn(...)') have a
    # different 'match.call()[[1]]' shape (a call to the '::'/':::'
    # operator) than plain 'fn(...)' calls; 'typetracer_header()' must
    # normalize this before passing it to 'match.fun()'. Exercise this with
    # a real namespace-qualified call to a real exported function from
    # another package ('rematch', already used elsewhere in this suite as a
    # toy target package). 'inject_tracer()' mutates the closure's internals
    # in place (not via 'assign()'), so this doesn't require any changes to
    # 'rematch''s (locked) namespace environment itself.
    skip_if_not_installed ("rematch")

    # 'match.fun()' resolves a bare function name starting from the caller's
    # frame, so (matching how a real traced package like 'pkgstats' is
    # attached before its own internal 'pkgstats::fn()'-style calls run) the
    # package must be attached here too for the qualified call below to
    # resolve correctly.
    library (rematch)
    on.exit (
        suppressWarnings (detach ("package:rematch", unload = TRUE)),
        add = TRUE
    )

    f <- getFromNamespace ("re_match", "rematch")

    clear_traces ()
    inject_tracer (f)
    on.exit (uninject_tracer (f), add = TRUE)

    val <- rematch::re_match (pattern = "^(a+)(b+)$", text = "aaabb")
    expect_equal (
        unname (val [1, ]),
        c ("aaabb", "aaa", "bb")
    )

    x <- load_traces (files = TRUE)
    expect_s3_class (x, "tbl_df")
    expect_true (nrow (x) > 0L)
    expect_true (all (x$fn_name == "re_match"))

    # Plain, unqualified (bare-symbol) calls to the same injected function
    # must still work exactly as before (called here as 'f', its local
    # variable name, since that's the literal call-site symbol captured by
    # 'match.call()' in this path):
    clear_traces ()
    val2 <- f (pattern = "^(a+)(b+)$", text = "aaabb")
    expect_equal (val2, val)
    x2 <- load_traces (files = TRUE)
    expect_true (all (x2$fn_name == "f"))
})

test_that ("trace call with unset typetracedir option", {

    # 'typetracedir' is set in .onLoad(), so is unset in contexts where the
    # 'typetracer' package itself was never loaded in the current process
    # (e.g. a traced closure executed in a fresh subprocess via
    # 'callr::r_bg()'). The header must fall back to 'tempdir()' rather than
    # erroring, matching 'get_typetrace_dir()''s existing behaviour.
    old_opt <- getOption ("typetracedir")
    withr::defer (options (typetracedir = old_opt))
    options (typetracedir = NULL)

    f <- function (x, y) {
        x * x + y * y
    }

    clear_traces ()
    inject_tracer (f)
    on.exit (uninject_tracer (f), add = TRUE)

    val <- f (x = 1:2, y = 3:4 + 0.)
    expect_equal (val, (1:2)^2 + (3:4 + 0.)^2)

    x <- load_traces (files = TRUE)
    expect_s3_class (x, "tbl_df")
    expect_equal (nrow (x), 2L)
})

test_that ("trace call via do.call with a function value", {

    # Some invocation mechanisms (e.g. 'callr::r_bg()') call the traced
    # function by passing the function *value* directly, rather than by
    # name, which 'do.call()' also does when its first argument is an
    # actual function object rather than a string/symbol. In that case
    # 'match.call()[[1]]' inside the header is the closure itself, not a
    # symbol or a '::'/':::' call.
    f <- function (x, y) {
        x * x + y * y
    }

    clear_traces ()
    inject_tracer (f)
    on.exit (uninject_tracer (f), add = TRUE)

    val <- do.call (f, list (x = 1:2, y = 3:4 + 0.))
    expect_equal (val, (1:2)^2 + (3:4 + 0.)^2)

    x <- load_traces (files = TRUE)
    expect_s3_class (x, "tbl_df")
    expect_equal (nrow (x), 2L)
    expect_true (all (x$fn_name == "<unknown>"))
})

test_that ("inject_pkg_trace_fns resolves unexported functions", {

    # Packages passed to 'inject_pkg_trace_fns()'/'uninject_pkg_trace_fns()'
    # may include unexported functions (e.g. S3 methods registered via
    # 'NAMESPACE's `S3method()` but not `export()`ed). These are always
    # resolvable via the package's internal namespace even when (as under a
    # real installed-package load, unlike 'devtools::load_all()'s default
    # 'export_all = TRUE') they are not bound in the attached 'package:<x>'
    # environment. 're_match_all1()' is a real, already-existing unexported
    # function in 'rematch' (already used elsewhere in this suite as a toy
    # target package), used here rather than one of typetracer's own internal
    # functions to avoid any risk of self-referential tracing (several of
    # typetracer's own internals, e.g. 'get_param_str()', are themselves
    # called from within the injected header code while tracing is active).
    skip_if_not_installed ("rematch")

    ns <- asNamespace ("typetracer")
    rematch_ns <- asNamespace ("rematch")
    fn_name <- "re_match_all1"

    body0 <- body (get (fn_name, envir = rematch_ns))

    clear_traces ()
    trace_fns <- ns$inject_pkg_trace_fns (
        functions = fn_name,
        package = "rematch"
    )
    expect_equal (trace_fns, fn_name)

    fn <- get (fn_name, envir = rematch_ns)
    body1 <- body (fn)
    expect_false (identical (body0, body1))

    m <- regexpr ("(a+)(b+)", "aaabb", perl = TRUE)
    # Called via the local 'fn' variable, not ':::' - real unexported
    # functions like this are normally reached via S3 dispatch (e.g.
    # 'UseMethod()'), where the recorded call reflects the generic's own
    # (exported, findable) name, not a ':::'-qualified direct call. As in
    # the "trace call via namespace-qualified reference" test above, a bare
    # call like this records the call-site variable's own name, "fn".
    val <- fn (m, "aaabb")
    expect_equal (unname (val [1, ]), c ("aaabb", "aaa", "bb"))

    x <- load_traces (files = TRUE)
    expect_s3_class (x, "tbl_df")
    expect_true (nrow (x) > 0L)
    expect_true (all (x$fn_name == "fn"))

    ns$uninject_pkg_trace_fns (trace_fns, "rematch")
    body2 <- body (get (fn_name, envir = rematch_ns))
    expect_identical (body0, body2)
})

test_that ("untrace call", {

    f <- function (x, y) {
        x * x + y * y
    }
    body0 <- body (f)

    inject_tracer (f)
    body1 <- body (f)

    expect_true (uninject_tracer (f))
    body2 <- body (f)

    e0 <- as.character (as.expression (body0))
    e1 <- as.character (as.expression (body1))
    e2 <- as.character (as.expression (body2))

    expect_false (identical (e0, e1))

    expect_identical (e0, e2)
    expect_false (uninject_tracer (f))
})

test_that ("trace lists", {

    f <- function (x, y, a) {
        stopifnot (is.list (a))
        stopifnot ("x" %in% names (a))
        x * x + y * y + a$x
    }

    clear_traces ()
    inject_tracer (f, trace_lists = FALSE)
    val <- f (x = 1:2, y = 3:4 + 0., a = list (x = 4))
    x0 <- load_traces ()
    expect_true (uninject_tracer (f))

    clear_traces ()
    inject_tracer (f, trace_lists = TRUE)
    val <- f (x = 1:2, y = 3:4 + 0., a = list (x = 4))
    x1 <- load_traces ()
    expect_true (uninject_tracer (f))

    expect_true (nrow (x1) > nrow (x0))
    expect_false (any (grepl ("\\$", x0$par_name)))
    expect_true (any (grepl ("\\$", x1$par_name)))
})
