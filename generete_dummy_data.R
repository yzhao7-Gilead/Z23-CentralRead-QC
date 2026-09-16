################################################################################
# generete_dummy_data.R
#
# Reads SAS dataset (.sas7bdat) in an input folder, generates synthetic
# ("dummy") data for each dataset using {synthpop}, writes the dummy datasets
# back out as .sas7bdat files, and uploads them to a target OneDrive folder
# using {Microsoft365R}.
#
# Required packages: haven, synthpop, Microsoft365R, purrr, fs
################################################################################

# ---------------------------------------------------------------------------
# 0. Configuration -- loaded from config.yml (edit that file, not this script)
# ---------------------------------------------------------------------------
required_pkgs <- c("haven", "synthpop", "Microsoft365R", "purrr", "fs", "yaml")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(sprintf(
    "Missing required package(s): %s. Install with install.packages(c(%s)).",
    paste(missing_pkgs, collapse = ", "),
    paste(sprintf('"%s"', missing_pkgs), collapse = ", ")
  ))
}

load_config <- function(config_path = "config.yml") {
  if (!file.exists(config_path)) {
    stop(sprintf("Config file not found: %s", config_path))
  }
  cfg <- yaml::read_yaml(config_path)

  required_keys <- c("input_dir", "output_dir", "datasets", "n_rows", "seed",
                     "onedrive_type", "onedrive_dest")
  missing_keys <- setdiff(required_keys, names(cfg))
  if (length(missing_keys) > 0) {
    stop(sprintf("config.yml is missing required key(s): %s",
                 paste(missing_keys, collapse = ", ")))
  }
  cfg
}

# ---------------------------------------------------------------------------
# 1. Find SAS datasets in the input folder, optionally filtered by name
# ---------------------------------------------------------------------------
find_sas_files <- function(input_dir, datasets = NULL) {
  files <- fs::dir_ls(input_dir, glob = "*.sas7bdat", recurse = FALSE)
  if (length(files) == 0) {
    stop(sprintf("No .sas7bdat files found in '%s'.", input_dir))
  }

  if (!is.null(datasets)) {
    datasets <- unlist(datasets)
    names    <- fs::path_ext_remove(fs::path_file(files))
    files    <- files[tolower(names) %in% tolower(datasets)]

    missing <- setdiff(tolower(datasets), tolower(names))
    if (length(missing) > 0) {
      stop(sprintf("Dataset(s) not found in '%s': %s", input_dir,
                   paste(missing, collapse = ", ")))
    }
  }

  files
}

# ---------------------------------------------------------------------------
# 2. Generate a synthetic version of a single dataset
# ---------------------------------------------------------------------------
generate_dummy_dataset <- function(df, n_rows = NULL, seed = 1) {
  # Keep original variable labels (haven imports these as attributes) so
  # they can be reapplied after synthpop strips them.
  var_labels <- lapply(df, function(col) attr(col, "label"))

  # synthpop cannot synthesize columns that are entirely NA or constant;
  # drop them from synthesis and add them back afterwards unchanged.
  all_na    <- vapply(df, function(col) all(is.na(col)), logical(1))
  syn_cols  <- names(df)[!all_na]
  keep_cols <- names(df)[all_na]

  k <- if (is.null(n_rows)) nrow(df) else n_rows

  synth <- synthpop::syn(
    df[, syn_cols, drop = FALSE],
    k        = k,
    seed     = seed,
    print.flag = FALSE
  )
  dummy <- synth$syn

  # Re-attach columns that were skipped (all-NA), padded/truncated to k rows
  for (col in keep_cols) {
    dummy[[col]] <- rep(NA, k)
  }
  dummy <- dummy[, names(df), drop = FALSE]

  # Reapply variable labels for haven::write_sas
  for (col in names(dummy)) {
    label <- var_labels[[col]]
    if (!is.null(label)) attr(dummy[[col]], "label") <- label
  }

  dummy
}

# ---------------------------------------------------------------------------
# 3. Process every dataset: read -> synthesize -> write
# ---------------------------------------------------------------------------
process_all_datasets <- function(cfg) {
  fs::dir_create(cfg$output_dir)
  sas_files <- find_sas_files(cfg$input_dir, cfg$datasets)

  purrr::walk(sas_files, function(path) {
    name <- fs::path_ext_remove(fs::path_file(path))
    message(sprintf("Processing dataset: %s", name))

    df    <- haven::read_sas(path)
    dummy <- generate_dummy_dataset(df, n_rows = cfg$n_rows, seed = cfg$seed)

    out_path <- fs::path(cfg$output_dir, paste0(name, ".sas7bdat"))
    haven::write_sas(dummy, out_path)
    message(sprintf("  -> wrote %s", out_path))
  })

  sas_files
}

# ---------------------------------------------------------------------------
# 4. Upload the generated dummy datasets to OneDrive
# ---------------------------------------------------------------------------
upload_to_onedrive <- function(cfg) {
  od <- if (identical(cfg$onedrive_type, "personal")) {
    Microsoft365R::get_personal_onedrive()
  } else {
    Microsoft365R::get_business_onedrive()
  }

  # Create the destination folder (no-op if it already exists)
  tryCatch(
    od$create_folder(cfg$onedrive_dest),
    error = function(e) invisible(NULL)
  )

  dummy_files <- fs::dir_ls(cfg$output_dir, glob = "*.sas7bdat", recurse = FALSE)
  purrr::walk(dummy_files, function(path) {
    dest <- paste0(cfg$onedrive_dest, "/", fs::path_file(path))
    message(sprintf("Uploading %s -> OneDrive:%s", fs::path_file(path), dest))
    od$upload_file(src = path, dest = dest)
  })

  invisible(dummy_files)
}

# ---------------------------------------------------------------------------
# 5. Main
# ---------------------------------------------------------------------------
main <- function(cfg) {
  set.seed(cfg$seed)
  process_all_datasets(cfg)
  upload_to_onedrive(cfg)
  message("Done: dummy datasets generated and uploaded to OneDrive.")
}

if (sys.nframe() == 0) {
  # Path to the config file, relative to the working directory this script
  # is run from, or override via the CONFIG_PATH env var / command-line arg.
  args        <- commandArgs(trailingOnly = TRUE)
  config_path <- if (length(args) >= 1) args[[1]] else Sys.getenv("CONFIG_PATH", "config.yml")

  cfg <- load_config(config_path)
  main(cfg)
}
