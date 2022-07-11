# tracer body

    Code
      body
    Output
      {
          typetracer_env <- new.env(parent = emptyenv())
          typetracer_env$td <- options("typetracedir")
          typetracer_env$nm <- paste0(sample(c(letters, LETTERS), 8), 
              collapse = "")
          typetracer_env$fname <- file.path(typetracer_env$td, paste0("typetrace_", 
              typetracer_env$nm, ".Rds"))
          typetracer_env$trace_dir <- options("typetracedir")$typetracedir
          typetracer_env$num_traces <- length(list.files(typetracer_env$trace_dir, 
              pattern = "^typetrace\\_"))
          typetracer_env$fn_call <- match.call(expand.dots = TRUE)
          typetracer_env$fn_name <- typetracer_env$fn_call[[1]]
          typetracer_env$pars <- as.list(typetracer_env$fn_call[-1L])
          fn_env <- environment()
          typetracer_env$fn <- match.fun(typetracer_env$fn_name)
          typetracer_env$par_names <- methods::formalArgs(typetracer_env$fn)
          typetracer_env$par_formals <- formals(typetracer_env$fn)
          if ("..." %in% typetracer_env$par_names) {
              typetracer_env$dot_names <- names(typetracer_env$fn_call)
              typetracer_env$dot_names <- typetracer_env$dot_names[which(nzchar(typetracer_env$dot_names) & 
                  !typetracer_env$dot_names %in% typetracer_env$par_names)]
              typetracer_env$par_names <- c(typetracer_env$par_names, 
                  typetracer_env$dot_names)
          }
          typetracer_env$get_str <- function(x, max.length = 1000L) {
              r <- tryCatch(format(x), error = function(e) e)
              r <- if (inherits(r, "error")) {
                  tryCatch(as.character(x), error = function(e) e)
              }
              else {
                  paste(r, collapse = " ")
              }
              r <- if (inherits(r, "error")) {
                  tryCatch(utils::capture.output(x), error = function(e) e)
              }
              else {
                  paste(r, collapse = " ")
              }
              substr(r, 1L, max.length)
          }
          typetracer_env$data <- lapply(typetracer_env$par_names, function(p) {
              res <- NULL
              if (p %in% ls(fn_env)) {
                  res <- tryCatch(get(p, envir = fn_env, inherits = FALSE), 
                      error = function(e) NULL)
              }
              if (is.null(res)) {
                  res <- tryCatch(eval(typetracer_env$pars[[p]], envir = fn_env), 
                      error = function(e) NULL)
              }
              s <- "NULL"
              if (!is.null(res)) {
                  s <- typetracer_env$get_str(typetracer_env$pars[[p]])
                  if (length(s) > 1L) {
                      s <- paste0(s, collapse = "; ")
                  }
                  if (is.null(s)) {
                      s <- "NULL"
                  }
              }
              list(par = p, class = class(res), typeof = typeof(res), 
                  storage_mode = storage.mode(res), mode = mode(res), 
                  length = length(res), par_uneval = s, par_eval = res)
          })
          typetracer_env$data$fn_name <- as.character(typetracer_env$fn_name)
          typetracer_env$data$formals <- typetracer_env$par_formals
          typetracer_env$data$num_traces <- typetracer_env$num_traces
          saveRDS(typetracer_env$data, typetracer_env$fname)
          rm(typetracer_env)
      }

