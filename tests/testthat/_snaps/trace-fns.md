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
          typetracer_env$add_dotdotdot_params <- utils::getFromNamespace("add_dotdotdot_params", 
              "typetracer")
          typetracer_env <- typetracer_env$add_dotdotdot_params(typetracer_env)
          typetracer_env$get_str <- utils::getFromNamespace("get_param_str", 
              "typetracer")
          typetracer_env$trace_one_param <- utils::getFromNamespace("trace_one_param", 
              "typetracer")
          typetracer_env$trace_one_list <- utils::getFromNamespace("trace_one_list", 
              "typetracer")
          typetracer_env$get_trace_lists_param <- utils::getFromNamespace("get_trace_lists_param", 
              "typetracer")
          typetracer_env$data <- lapply(typetracer_env$par_names, function(p) {
              dat_i <- typetracer_env$trace_one_param(typetracer_env, 
                  p, fn_env)
              trace_lists <- typetracer_env$get_trace_lists_param()
              if (dat_i$typeof == "list" && trace_lists) {
                  dat_i$list_data <- typetracer_env$trace_one_list(typetracer_env, 
                      p, fn_env)
              }
              return(dat_i)
          })
          typetracer_env$process_back_trace <- utils::getFromNamespace("process_back_trace", 
              "typetracer")
          trace_dat <- rlang::trace_back(bottom = fn_env)
          typetracer_env$data$call_envs <- typetracer_env$process_back_trace(trace_dat, 
              typetracer_env$fn_name)
          typetracer_env$data$fn_name <- as.character(typetracer_env$fn_name)
          typetracer_env$data$par_formals <- typetracer_env$par_formals
          typetracer_env$data$num_traces <- typetracer_env$num_traces
          saveRDS(typetracer_env$data, typetracer_env$fname)
          rm(typetracer_env)
      }

