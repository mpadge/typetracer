# tracer body

    Code
      body
    Output
      {
          td <- options("typetracedir")
          nm <- paste0(sample(c(letters, LETTERS), 8), collapse = "")
          fname <- file.path(td, paste0("typetrace_", nm, ".txt"))
          con <- file(fname, open = "w")
          fn <- match.call()[[1]]
          pars <- formals(get(fn))
          for (p in names(pars)) {
              p_mode <- storage.mode(get(p))
              p_len <- length(get(p))
              out <- paste0(c(fn, p, p_mode, p_len), collapse = ",")
              writeLines(out, con)
          }
          close(con)
      }

