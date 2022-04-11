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
              typetracer_env$nm, ".txt"))
          typetracer_env$fn_call <- match.call(expand.dots = TRUE)
          typetracer_env$fn_name <- typetracer_env$fn_call[[1]]
          typetracer_env$pars <- as.list(typetracer_env$fn_call[-1L])
          fn_env <- environment()
          typetracer_env$fn <- match.fun(typetracer_env$fn_name)
          typetracer_env$par_names <- methods::formalArgs(typetracer_env$fn)
          typetracer_env$get_str <- function(x, max.length = 1000L) {
              r <- tryCatch(format(x), error = function(e) e)
              r <- if (inherits(r, "error")) 
                  tryCatch(as.character(x), error = function(e) e)
              else paste(r, collapse = " ")
              r <- if (inherits(r, "error")) 
                  tryCatch(utils::capture.output(x), error = function(e) e)
              else paste(r, collapse = " ")
              substr(r, 1L, max.length)
          }
          typetracer_env$classes <- vapply(typetracer_env$par_names, 
              function(p) {
                  res <- NULL
                  if (p %in% ls(fn_env)) {
                      res <- tryCatch(get(p, envir = fn_env, inherits = FALSE), 
                        error = function(e) NULL)
                  }
                  if (is.null(res)) {
                      res <- tryCatch(eval(typetracer_env$pars[[p]], 
                        envir = fn_env), error = function(e) NULL)
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
                  c(class(res)[1], storage.mode(res), length(res), 
                      s)
              }, character(4))
          typetracer_env$classes <- data.frame(t(typetracer_env$classes))
          colnames(typetracer_env$classes) <- c("class", "storage.mode", 
              "length", "structure")
          typetracer_env$classes$fn_name <- as.character(typetracer_env$fn_name)
          typetracer_env$classes$p <- typetracer_env$par_names
          typetracer_env$cols <- c("fn_name", "p", "class", "storage.mode", 
              "length", "structure")
          typetracer_env$classes <- typetracer_env$classes[, typetracer_env$cols]
          write_output <- function(fname, classes) {
              con <- file(fname, open = "at")
              on.exit(close(con, type = "at"))
              apply(classes, 1, function(i) writeLines(paste0(i, collapse = ","), 
                  con = con))
          }
          write_output(typetracer_env$fname, typetracer_env$classes)
          rm(typetracer_env)
      }

