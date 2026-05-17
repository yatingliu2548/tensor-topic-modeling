find_ttm_repo_root_safe <- function(start = getwd()) {
  cur <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(cur, "methods", "score_based_methods.R")) &&
      file.exists(file.path(cur, "methods", "analysis_function.R")) &&
      dir.exists(file.path(cur, "methods", "vh_methods"))
    ) {
      return(cur)
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) {
      return(NULL)
    }
    cur <- parent
  }
}

ArchetypeA <- function(R, K) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("VHMethod='AA' requires the R package 'reticulate'")
  }

  repo_root <- tryCatch(find_ttm_repo_root_safe(), error = function(e) NULL)
  nmf_candidates <- c(
    Sys.getenv("AA_NMF_PY", unset = ""),
    file.path(getwd(), "NMF.py"),
    file.path(getwd(), "methods", "vh_methods", "NMF.py"),
    file.path(getwd(), "..", "topic-modeling", "NMF.py"),
    if (!is.null(repo_root)) file.path(repo_root, "NMF.py") else "",
    if (!is.null(repo_root)) file.path(repo_root, "methods", "vh_methods", "NMF.py") else "",
    if (!is.null(repo_root)) file.path(dirname(repo_root), "topic-modeling", "NMF.py") else ""
  )
  nmf_candidates <- unique(nmf_candidates[nzchar(nmf_candidates)])
  nmf_py <- nmf_candidates[file.exists(nmf_candidates)][1]
  if (is.na(nmf_py) || !nzchar(nmf_py)) {
    stop("VHMethod='AA' could not find NMF.py. Set AA_NMF_PY or place NMF.py in a known location.")
  }

  py_candidates <- c(
    Sys.getenv("RETICULATE_PYTHON", unset = ""),
    Sys.getenv("AA_PYTHON", unset = ""),
    if (nzchar(Sys.getenv("R_CONDA_PREFIX", unset = ""))) {
      file.path(Sys.getenv("R_CONDA_PREFIX"), "bin", "python")
    } else {
      ""
    },
    Sys.which("python3"),
    Sys.which("python")
  )
  py_candidates <- unique(py_candidates[nzchar(py_candidates)])
  py_bin <- py_candidates[file.exists(py_candidates)][1]
  if (is.na(py_bin) || !nzchar(py_bin)) {
    stop("VHMethod='AA' requires a Python interpreter. Set RETICULATE_PYTHON or AA_PYTHON.")
  }

  reticulate::use_python(py_bin, required = TRUE)
  missing_modules <- c("numpy", "scipy", "matplotlib")[!vapply(
    c("numpy", "scipy", "matplotlib"),
    reticulate::py_module_available,
    logical(1)
  )]
  if (length(missing_modules) > 0) {
    stop(
      sprintf(
        "VHMethod='AA' is missing Python modules in %s: %s",
        py_bin,
        paste(missing_modules, collapse = ", ")
      )
    )
  }

  reticulate::source_python(nmf_py)
  resultfromAA <- acc_palm_nmf(
    X = reticulate::r_to_py(R),
    r = K,
    proj_method = "wolfe",
    m = 5,
    maxiter = 1000,
    c1 = 1,
    c2 = 1,
    method = "fista",
    fixed_max_size = 5
  )
  list(V = resultfromAA$V, Weigh_hat = resultfromAA$weight)
}
