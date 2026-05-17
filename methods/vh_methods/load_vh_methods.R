resolve_vh_methods_dir <- function() {
  this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE), error = function(e) "")
  if (nzchar(this_file)) {
    return(dirname(this_file))
  }

  cur <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    candidate <- file.path(cur, "methods", "vh_methods")
    if (dir.exists(candidate)) {
      return(candidate)
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) {
      stop("Cannot locate methods/vh_methods")
    }
    cur <- parent
  }
}

load_vh_methods <- local({
  initialized <- FALSE
  function() {
    if (initialized) {
      return(invisible(TRUE))
    }

    vh_dir <- resolve_vh_methods_dir()
    source(file.path(dirname(vh_dir), "analysis_function.R"), local = globalenv())
    source(file.path(vh_dir, "sp.R"), local = globalenv())
    source(file.path(vh_dir, "svs.R"), local = globalenv())
    source(file.path(vh_dir, "svs_sp.R"), local = globalenv())
    source(file.path(vh_dir, "aa.R"), local = globalenv())

    initialized <<- TRUE
    invisible(TRUE)
  }
})

load_vh_methods()
