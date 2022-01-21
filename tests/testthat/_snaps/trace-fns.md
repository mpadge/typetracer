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
          par_names <- formalArgs(fn)
          get_p <- function(p, fn_env) {
              tryCatch(get(p, envir = fn_env, inherits = FALSE, silent = TRUE), 
                  error = function(e) NULL)
          }
          eval_p <- function(p, pars) {
              tryCatch(eval(pars[[p]]), error = function(e) NULL)
          }
          classes1 <- vapply(par_names, function(p) {
              res <- eval_p(p, pars)
              if (is.null(res)) 
                  res <- get_p(p, fn_env)
              c(class(res)[1], storage.mode(res), length(res))
          }, character(3))
          classes1 <- t(classes1)
          no_class <- which(classes1[, 1] == "NULL")
          classes2 <- vapply(par_names, function(p) {
              res <- eval_p(p, fn_env)
              c(class(res)[1], storage.mode(res), length(res))
          }, character(3))
          classes2 <- t(classes2)
          classes1[no_class, ] <- classes2[no_class, ]
          classes <- data.frame(classes1)
          colnames(classes) <- c("class", "storage.mode", "length")
          classes$fn_name <- as.character(fn_name)
          classes$p <- par_names
          classes <- classes[, c("fn_name", "p", "storage.mode", "length")]
          classes <- apply(classes, 1, function(i) {
              out <- paste0(i, collapse = ",")
              writeLines(out, typetracer_con)
          })
          close(typetracer_con)
          rm(td, nm, fn, fname, typetracer_con, fn_call, fn_name, fn_env, 
              pars, par_names, get_p, eval_p, classes1, classes2, no_class)
      }

