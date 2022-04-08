# tracer body

    Code
      body
    Output
      {
          tt_env <- new.env(parent = emptyenv())
          tt_env$td <- options("typetracedir")
          tt_env$nm <- paste0(sample(c(letters, LETTERS), 8), collapse = "")
          tt_env$fname <- file.path(tt_env$td, paste0("typetrace_", 
              tt_env$nm, ".txt"))
          tt_env$typetracer_con <- file(tt_env$fname, open = "at")
          tt_env$fn_call <- match.call(expand.dots = TRUE)
          tt_env$fn_name <- tt_env$fn_call[[1]]
          tt_env$pars <- as.list(tt_env$fn_call[-1L])
          fn_env <- environment()
          tt_env$fn <- match.fun(tt_env$fn_name)
          tt_env$par_names <- methods::formalArgs(tt_env$fn)
          tt_env$classes <- vapply(tt_env$par_names, function(p) {
              res <- NULL
              if (p %in% ls(fn_env)) {
                  res <- tryCatch(get(p, envir = fn_env, inherits = FALSE), 
                      error = function(e) NULL)
              }
              if (is.null(res)) {
                  res <- tryCatch(eval(tt_env$pars[[p]], envir = fn_env), 
                      error = function(e) NULL)
              }
              c(class(res)[1], storage.mode(res), length(res))
          }, character(3))
          tt_env$classes <- data.frame(t(tt_env$classes))
          colnames(tt_env$classes) <- c("class", "storage.mode", "length")
          tt_env$classes$fn_name <- as.character(tt_env$fn_name)
          tt_env$classes$p <- tt_env$par_names
          tt_env$cols <- c("fn_name", "p", "class", "storage.mode", 
              "length")
          tt_env$classes <- tt_env$classes[, tt_env$cols]
          apply(tt_env$classes, 1, function(i) {
              writeLines(paste0(i, collapse = ","), tt_env$typetracer_con)
          })
          close(tt_env$typetracer_con)
          rm(tt_env)
      }

