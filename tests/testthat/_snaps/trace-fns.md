# tracer body

    Code
      body
    Output
      {
          td <- options("typetracedir")
          nm <- paste0(sample(c(letters, LETTERS), 8), collapse = "")
          fname <- file.path(td, paste0("typetrace_", nm, ".txt"))
          typetracer_con <- file(fname, open = "at")
          fn_call <- match.call(expand.dots = TRUE)
          fn_name <- fn_call[[1]]
          pars <- as.list(fn_call[-1L])
          fn_env <- environment()
          fn <- match.fun(fn_name)
          par_names <- methods::formalArgs(fn)
          classes <- vapply(par_names, function(p) {
              res <- NULL
              if (p %in% ls(fn_env)) {
                  res <- tryCatch(get(p, envir = fn_env, inherits = FALSE), 
                      error = function(e) NULL)
              }
              if (is.null(res)) {
                  res <- tryCatch(eval(pars[[p]], envir = fn_env), 
                      error = function(e) NULL)
              }
              c(class(res)[1], storage.mode(res), length(res))
          }, character(3))
          classes <- data.frame(t(classes))
          colnames(classes) <- c("class", "storage.mode", "length")
          classes$fn_name <- as.character(fn_name)
          classes$p <- par_names
          classes <- classes[, c("fn_name", "p", "class", "storage.mode", 
              "length")]
          apply(classes, 1, function(i) {
              out <- paste0(i, collapse = ",")
              writeLines(out, typetracer_con)
          })
          close(typetracer_con)
          rm(td, nm, fn, fname, typetracer_con, fn_call, fn_name, fn_env, 
              pars, par_names, classes)
      }

