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
          typetracer_env$get_str <- getFromNamespace("get_param_str", 
              "typetracer")
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
              trace_dat <- rlang::trace_back(bottom = fn_env)
              trace_dat <- trace_dat[which(trace_dat$parent == 0), 
                  ]
              call_envs <- lapply(trace_dat$call, function(i) {
                  call_i <- data.frame(name = as.character(as.name(as.list(i)[[1]])), 
                      file = NA_character_, linestart = NA_integer_, 
                      lineend = NA_integer_)
                  if (!is.null(attributes(i)$srcref)) {
                      call_i$file <- attr(attributes(i)$srcref, "srcfile")$filename
                      call_i$linestart <- attr(i, "srcref")[1]
                      call_i$lineend <- attr(i, "srcref")[3]
                  }
                  return(call_i)
              })
              call_envs <- do.call(rbind, call_envs)
              call_envs$namespace <- trace_dat$namespace
              index <- which(is.na(call_envs$namespaces))
              if (length(index) > 0L) {
                  call_envs$namespace[index] <- trace_dat$scope[index]
              }
              call_envs <- call_envs[which(call_envs$namespace != "typetracer"), 
                  ]
              if (nrow(call_envs) > 0L) {
                  call_envs <- call_envs[1, ]
              }
              list(par = p, class = class(res), typeof = typeof(res), 
                  storage_mode = storage.mode(res), mode = mode(res), 
                  length = length(res), par_uneval = s, par_eval = res, 
                  call_envs = call_envs)
          })
          typetracer_env$data$fn_name <- as.character(typetracer_env$fn_name)
          typetracer_env$data$formals <- typetracer_env$par_formals
          typetracer_env$data$num_traces <- typetracer_env$num_traces
          saveRDS(typetracer_env$data, typetracer_env$fname)
          rm(typetracer_env)
      }

