#' Create a file mapping for multi-repository deployment
#'
#' @param ... Named arguments where names are local file paths and values are repository paths
#' @param dir Character string specifying a directory to search for files. Default is NULL.
#' @param pattern Character string with a regular expression pattern to match files in dir. Default is NULL.
#' @param target_prefix Character string to prefix to all target paths. Default is "".
#' @param preserve_structure Logical indicating whether to preserve directory structure in target. Default is FALSE.
#'
#' @return A named list where names are local file paths and values are repository paths
#' @export
#'
#' @examples
#' \dontrun{
#' # Individual files
#' mapping <- file_mapping(
#'   "local/path/ci.yml" = ".github/workflows/ci.yml",
#'   "local/path/lint.R" = ".lintr"
#' )
#'
#' # All yaml files from a directory to .github/workflows
#' mapping <- file_mapping(
#'   dir = "local/workflows",
#'   pattern = "\\.ya?ml$",
#'   target_prefix = ".github/workflows/"
#' )
#'
#' # Preserve directory structure
#' mapping <- file_mapping(
#'   dir = "templates",
#'   preserve_structure = TRUE
#' )
#' }
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
#' @param x A file_mapping object to print
#' @param max_files Maximum number of files to display. Default is 20.
#' @param ... Additional arguments passed to print methods (not used)
#'
#' @return Invisibly returns the file_mapping object
#' @export
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
