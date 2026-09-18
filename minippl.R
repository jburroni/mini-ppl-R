# Loader for the self-contained MiniPPL project.

.minippl_target <- environment()
.minippl_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
.minippl_dir <- if (is.null(.minippl_file)) {
  getwd()
} else {
  dirname(normalizePath(.minippl_file, mustWork = TRUE))
}

sys.source(file.path(.minippl_dir, "prelude.R"), envir = .minippl_target)
sys.source(file.path(.minippl_dir, "runtime.R"), envir = .minippl_target)

rm(.minippl_file, .minippl_dir, .minippl_target)
