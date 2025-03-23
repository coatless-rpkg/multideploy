#' Create a file mapping for multi-repository deployment
#'
#' This function builds a mapping between local files and their target paths in repositories,
#' supporting both individual file mapping and bulk directory processing.
#'
#' @param ... Named arguments where names are local file paths and values are repository paths
#' @param dir Character string specifying a directory to search for files. Default is NULL.
#' @param pattern Character string with a regular expression pattern to match files in dir. Default is NULL.
#' @param target_prefix Character string to prefix to all target paths. Default is "".
#' @param preserve_structure Logical indicating whether to preserve directory structure in target. Default is FALSE.
#' @param quiet Logical; if TRUE, suppresses information messages. Default is FALSE.
#'
#' @return 
#' Returns an object of class `"file_mapping"` (which is just a marked up `"list"`) containing:
#' 
#' - A named list where each entry maps a local file path (name) to a target repository path (value)
#' - Each key is the full path to a local file
#' - Each value is the corresponding path where the file should be placed in repositories
#'
#' @seealso [`print.file_mapping()`] to display the mapping in a formatted way.
#' 
#' @details
#' The `dir` argument requires a valid directory path currently on the local filesystem.
#' This directory is scanned for files matching the `pattern` regular expression,
#' and each file is mapped to a target path in repositories. If the directory is
#' not found, an error is thrown.
#' 
#' 
#' @export
#'
#' @examples
#' # Map individual files with explicit source-to-target paths
#' mapping <- file_mapping(
#'   "local/path/ci.yml" = ".github/workflows/ci.yml",
#'   "local/path/lint.R" = ".lintr"
#' )
#'
#' # Automatically map all R files from a directory to backup/R2/
#' workflow_mapping <- file_mapping(
#'   dir = system.file(package = "multideploy"),
#'   pattern = "\\.R$",
#'   target_prefix = "backup/R2/"
#' )
#'
#' # Preserve directory structure when mapping files
#' template_mapping <- file_mapping(
#'   dir = system.file(package = "multideploy"),
#'   preserve_structure = TRUE
#' )
#'
#' # Combine explicit mappings with directory-based mappings
#' combined_mapping <- file_mapping(
#'   "specific/file.R" = "R/functions.R",
#'   dir = system.file(package = "multideploy"),
#'   target_prefix = ".github/"
#' )
file_mapping <- function(..., dir = NULL, pattern = NULL, 
                         target_prefix = "", preserve_structure = FALSE, quiet = FALSE) {
  mapping <- list(...)
  
  # Process files from directory if specified
  if (!is.null(dir)) {
    if (!dir.exists(dir)) {
      cli::cli_abort("Directory does not exist: {.file {dir}}")
    }
    
    # Get all files in directory
    files <- list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
    
    if (length(files) == 0) {
      cli::cli_alert_warning("No files found in directory {.file {dir}}")
      return(mapping)
    }
    
    for (file in files) {
      if (preserve_structure) {
        # Preserve directory structure
        rel_path <- sub(paste0("^", dir, "/"), "", file)
        mapping[[file]] <- paste0(target_prefix, rel_path)
      } else {
        # Flatten structure
        file_name <- basename(file)
        mapping[[file]] <- paste0(target_prefix, file_name)
      }
    }
  }
  
  # Validate mapping
  for (local_file in names(mapping)) {
    if (!file.exists(local_file)) {
      cli::cli_alert_warning("Local file does not exist: {.file {local_file}}")
    }
  }

  cli::cli_alert_info("Created file mapping with {length(mapping)} files")
  
  # Assign class to enable S3 method dispatch
  class(mapping) <- c("file_mapping", "list")
  
  return(mapping)
}


#' Print method for file_mapping objects
#'
#' This method provides a formatted display of file mappings, showing the relationship between
#' local files and their target repository paths with visual indicators for file existence.
#'
#' @param x An object of class `"file_mapping"` as returned by `file_mapping()`
#' @param max_files Maximum number of files to display. Default is 20.
#' @param ... Additional arguments passed to other print methods (not used)
#'
#' @return 
#' Invisibly returns the original `file_mapping` object unchanged, allowing for
#' chained operations. 
#' 
#' Displays a formatted representation of the mapping to the console, including:
#' 
#' - Total count of mapped files
#' - Visual indicators showing which local files exist (checkmark) or are missing (x)
#' - Source-to-target mapping for each file (limited by `max_files`)
#'
#' @export
#'
#' @examples
#' # Create and display a mapping
#' mapping <- file_mapping(
#'   "R/functions.R" = "R/utils.R",
#'   dir = system.file(package = "multideploy")
#' )
#' # The mapping is automatically printed when not assigned
#'
#' # Control how many files are displayed
#' mapping <- file_mapping(dir = system.file(package = "multideploy"))
#' print(mapping, max_files = 5)  # Show only first 5 mappings
print.file_mapping <- function(x, max_files = 20, ...) {
  n_files <- length(x)
  
  # Print header
  cli::cli_h2("File Mapping")
  cli::cli_text("{n_files} file{?s} mapped for deployment")
  
  # Print each mapping entry
  if (n_files > 0) {
    display_count <- min(n_files, max_files)
    
    # Add newline before the list
    cli::cli_text("")
    
    # Create a bullet list of mappings
    cli::cli_ul()
    
    for (i in seq_len(display_count)) {
      local_file <- names(x)[i]
      repo_path <- x[[i]]
      
      # Check if local file exists
      file_exists <- file.exists(local_file)
      
      # Use different styling based on file existence
      if (file_exists) {
        status <- cli::col_green(cli::symbol$tick)
      } else {
        status <- cli::col_red(cli::symbol$cross)
      }
      
      # Use bullets for each mapping with existence indicator
      cli::cli_li("{status} {.file {local_file}} {cli::symbol$arrow_right} {.path {repo_path}}")
    }
    
    # End bullet list
    cli::cli_end()
    
    # Add newline after the list
    cli::cli_text("")
    
    # Add note explaining symbols at the end
    cli::cli_text("Note: {cli::col_green(cli::symbol$tick)} indicates file exists, {cli::col_red(cli::symbol$cross)} indicates file not found")
    
    # If we didn't show all files, indicate how many more there are
    if (n_files > max_files) {
      remaining <- n_files - max_files
      cli::cli_alert_info("... and {remaining} more file{?s} not shown")
    }
  } else {
    cli::cli_alert_warning("No files in mapping")
  }
  
  # Return invisibly
  invisible(x)
}
