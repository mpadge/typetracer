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
          par_names <- names(pars)
          fn_env <- environment()
          for (p in par_names) {
              if (p %in% names(fn_env)) {
                  p_eval <- get(p, envir = fn_env)
              }
              else if (p %in% names(pars)) {
                  p_eval <- tryCatch(eval(pars[[p]]), error = function(e) NULL)
              }
              if (!is.null(p_eval)) {
                  p_mode <- storage.mode(p_eval)
                  p_len <- length(p_eval)
                  out <- paste0(c(fn_name, p, p_mode, p_len), collapse = ",")
                  writeLines(out, typetracer_con)
                  rm(p_mode, p_len, out, p_eval)
              }
          }
          close(typetracer_con)
          rm(td, nm, fname, typetracer_con, fn_call, fn_name, pars, 
              par_names, fn_env, p)
      }

