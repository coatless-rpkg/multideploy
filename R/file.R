#' Retrieve the content of a file from a GitHub repository
#'
#' @param repo Character string specifying the full name of the repository (owner/repo)
#' @param path Character string specifying the path to the file
#' @param ref Character string specifying the branch name or commit SHA. Default is NULL.
#'
#' @return List with the content and SHA of the file if it exists, NULL otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' file_info <- file_content("username/repo", "path/to/file.R")
#' }
file_content <- function(repo, path, ref = NULL, quiet = FALSE) {
  tryCatch({
    query <- list(path = path)
    if (!is.null(ref)) {
      query$ref <- ref
    }
    
    result <- gh::gh("GET /repos/{repo}/contents/{path}",
                     repo = repo,
                     path = path,
                     ref = ref)
    
    if (is.list(result) && !is.null(result$content)) {
      content <- rawToChar(base64enc::base64decode(gsub("\n", "", result$content)))
      return(list(
        content = content,
        sha = result$sha
      ))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    if (!quiet) {
      cli::cli_alert_warning("Could not fetch file {.file {path}} from {.val {repo}}: {e$message}")
    }
    return(NULL)
  })
}

#' Create or update a file in a GitHub repository
#'
#' @param repo Character string specifying the full name of the repository (owner/repo)
#' @param path Character string specifying the path to the file
#' @param content Character string with the new content of the file
#' @param message Character string with the commit message
#' @param branch Character string specifying the branch name. Default is NULL (uses default branch).
#' @param sha Character string with the blob SHA of the file being replaced (required for updates). Default is NULL.
#'
#' @return A list with the API response or NULL if operation failed
#' @export
#'
#' @examples
#' \dontrun{
#' result <- file_update("username/repo", "path/to/file.R", "new content", "Update file")
#' }
file_update <- function(repo, path, content, message, branch = NULL, sha = NULL, quiet = FALSE) {
  tryCatch({
    # Determine if create or update operation
    operation <- if (is.null(sha)) "create" else "update"
    
    # Construct query parameters
    query <- list(
      repo = repo,
      path = path,
      message = message,
      content = base64enc::base64encode(charToRaw(content))
    )
    
    if (!is.null(branch)) {
      query$branch <- branch
    }
    
    if (!is.null(sha)) {
      query$sha <- sha
    }
    
    # API call
    result <- do.call(gh::gh, c(
      list(
        endpoint = paste0("PUT /repos/{repo}/contents/{path}")
      ),
      query
    ))
    
    if (!quiet) {
      if (operation == "create") {
        cli::cli_alert_success("Created file {.file {path}} in {.val {repo}}")
      } else {
        cli::cli_alert_success("Updated file {.file {path}} in {.val {repo}}")
      }
    }
    
    return(result)
    
  }, error = function(e) {
    operation_name <- ifelse(is.null(sha), "creating", "updating")
    if (!quiet) {
      cli::cli_alert_danger("Error {operation_name} file {.file {path}} in {.val {repo}}: {e$message}")
    }
    return(NULL)
  })
}

#' Deploy a file to multiple GitHub repositories
#'
#' @param source_file Character string specifying the local file path to deploy
#' @param target_path Character string specifying the path in the repositories where the file should be placed
#' @param repos Data frame of repositories as returned by repos()
#' @param commit_message Character string with the commit message. Default uses a standard message.
#' @param branch Character string specifying the branch name. Default is NULL (uses default branch).
#' @param create_if_missing Logical indicating whether to create the file if it doesn't exist. Default is TRUE.
#' @param dry_run Logical indicating whether to only simulate the changes without actually making them. Default is FALSE.
#'
#' @return A data frame with the results of each deployment
#' @export
#'
#' @examples
#' \dontrun{
#' repositories <- repos("myorg")
#' results <- file_deploy("local/path/to/file.R", ".github/workflows/ci.yml", repositories)
#' }
file_deploy <- function(source_file, target_path, repos, 
                        commit_message = NULL, branch = NULL, 
                        create_if_missing = TRUE, dry_run = FALSE, quiet = FALSE) {
  
  # Read the local file
  if (!file.exists(source_file)) {
    cli::cli_abort("Source file does not exist: {.file {source_file}}")
  }
  
  file_content <- readChar(source_file, file.info(source_file)$size)
  
  if (is.null(commit_message)) {
    commit_message <- sprintf("Update %s via multi.gh automated deployment", target_path)
  }
  
  # Initialize results data frame
  results <- data.frame(
    repository = repos$full_name,
    status = NA_character_,
    message = NA_character_,
    stringsAsFactors = FALSE
  )
  
  if (!quiet) {
    cli::cli_h1("File Deployment")
    cli::cli_alert_info("Source: {.file {source_file}}")
    cli::cli_alert_info("Target: {.file {target_path}}")
    cli::cli_alert_info("Repositories: {.val {nrow(repos)}}")
    if (dry_run) {
      cli::cli_alert_warning("DRY RUN MODE - No changes will be made")
    }
    
    cli::cli_progress_bar("Processing repositories", total = nrow(repos))
  }
  
  for (i in seq_len(nrow(repos))) {
    repo <- repos$full_name[i]
    
    cli::cli_progress_update()
    
    tryCatch({
      # Get the current file content and SHA (if it exists)
      current <- file_content(repo, target_path, ref = branch)
      
      if (is.null(current)) {
        # File doesn't exist
        if (!create_if_missing) {
          results$status[i] <- "skipped"
          results$message[i] <- "File doesn't exist and create_if_missing is FALSE"
          next
        }
        
        if (!dry_run) {
          file_update(repo, target_path, file_content, commit_message, branch = branch, quiet = quiet)
        }
        results$status[i] <- if (dry_run) "would_create" else "created"
        results$message[i] <- "File created"
      } else {
        # File exists - check if content is different
        if (identical(current$content, file_content)) {
          results$status[i] <- "unchanged"
          results$message[i] <- "File content is identical, no update needed"
          next
        }
        
        if (!dry_run) {
          file_update(repo, target_path, file_content, commit_message, 
                      branch = branch, sha = current$sha, quiet = quiet)
        }
        results$status[i] <- if (dry_run) "would_update" else "updated"
        results$message[i] <- "File updated"
      }
    }, error = function(e) {
      results$status[i] <- "error"
      results$message[i] <- paste("Error:", e$message)
    })
  }
  
  if (!quiet) {
    cli::cli_progress_done()
  }
  
  # Set class for custom print method
  class(results) <- c("file_deploy_result", class(results))
  
  # Print summary if not quiet
  if (!quiet) {
    print(results)
  }
  
  return(results)
}

#' Print method for file_deploy_result objects
#'
#' @param x The file_deploy_result object to print
#' @param ... Additional arguments to pass to print methods
#'
#' @return Invisibly returns the input data frame
#' @export
#'
#' @examples
#' \dontrun{
#' results <- file_deploy("local/file.R", "remote/file.R", repos)
#' print(results)
#' }
print.file_deploy_result <- function(x, ...) {
  # Summary
  status_counts <- table(x$status)
  cli::cli_h2("Deployment Summary")
  
  # Print status counts with appropriate styling
  for (status in names(status_counts)) {
    count <- status_counts[[status]]
    
    # Choose appropriate alert type based on status
    alert_fn <- switch(status,
                       created = cli::cli_alert_success,
                       updated = cli::cli_alert_success,
                       unchanged = cli::cli_alert_info,
                       skipped = cli::cli_alert_warning,
                       would_create = cli::cli_alert_warning,
                       would_update = cli::cli_alert_warning,
                       error = cli::cli_alert_danger,
                       cli::cli_alert_info)
    
    # Call the alert function
    alert_fn("{status}: {count} repositories")
  }
  
  # Show errors if any
  errors <- x[x$status == "error", ]
  if (nrow(errors) > 0) {
    cli::cli_h3("Error Details")
    for (i in seq_len(nrow(errors))) {
      cli::cli_alert_danger("{errors$repository[i]}: {errors$message[i]}")
    }
  }
  
  invisible(x)
}