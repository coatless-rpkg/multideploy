#' Retrieve the content of a file from a GitHub repository
#'
#' This function fetches a file from a GitHub repository and returns its content and SHA.
#' If the file cannot be retrieved, it returns NULL and optionally displays a warning message.
#'
#' @param repo Character string specifying the full name of the repository (format: "owner/repo")
#' @param path Character string specifying the path to the file within the repository
#' @param ref Character string specifying the branch name, tag, or commit SHA. Default is NULL (uses default branch).
#'
#' @return 
#' When successful, returns a `list` with two elements:
#' 
#' \describe{
#'   \item{content}{Character string containing the decoded file content}
#'   \item{sha}{Character string with the file's blob SHA for use in update operations}
#' }
#' 
#' When the file cannot be retrieved (e.g., does not exist or no access), returns `NULL`.
#'
#' @export
#'
#' @examplesIf interactive()
#' # Get content from default branch
#' file_info <- file_content("username/repository", "path/to/file.R")
#' if (!is.null(file_info)) {
#'   # Access the content and SHA
#'   content <- file_info$content
#'   sha <- file_info$sha
#' }
#'
#' # Get content from specific branch
#' file_info <- file_content("username/repository", "path/to/file.R", ref = "develop")
#'
#' # Suppress warnings
#' file_info <- file_content("username/repository", "path/to/file.R")
file_content <- function(repo, path, ref = NULL) {
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
    cli::cli_alert_warning("Could not fetch file {.file {path}} from {.val {repo}}: {e$message}")
    return(NULL)
  })
}

#' Create or update a file in a GitHub repository
#'
#' This function creates a new file or updates an existing file in a GitHub repository.
#' For updating existing files, the SHA of the current file must be provided.
#'
#' @param repo Character string specifying the full name of the repository (format: "owner/repo")
#' @param path Character string specifying the path to the file within the repository
#' @param content Character string with the new content of the file
#' @param message Character string with the commit message
#' @param branch Character string specifying the branch name. Default is NULL (uses default branch).
#' @param sha Character string with the blob SHA of the file being replaced. Required for updating
#'   existing files; omit for creating new files. Default is NULL.
#' @param quiet Logical; if TRUE, suppresses progress and status messages. Default is FALSE.
#'
#' @return 
#' When successful, returns a `list` containing the GitHub API response with details about the commit,
#' including:
#' 
#' \describe{
#'   \item{content}{Information about the updated file}
#'   \item{commit}{Details about the created commit}
#' }
#' 
#' When the operation fails (e.g., permission issues, invalid SHA), returns `NULL`.
#'
#' @export
#'
#' @examplesIf interactive()
#' # Create a new file
#' result <- file_update(
#'   repo = "username/repository", 
#'   path = "path/to/new_file.R", 
#'   content = "# New R script\n\nprint('Hello world')", 
#'   message = "Add new script file"
#' )
#' # Check if operation was successful
#' if (!is.null(result)) {
#'   # Access commit information
#'   commit_sha <- result$commit$sha
#' }
#'
#' # Update an existing file (requires SHA)
#' file_info <- file_content("username/repository", "path/to/existing_file.R")
#' if (!is.null(file_info)) {
#'   result <- file_update(
#'     repo = "username/repository", 
#'     path = "path/to/existing_file.R", 
#'     content = "# Updated content\n\nprint('Hello updated world')", 
#'     message = "Update file content",
#'     sha = file_info$sha
#'   )
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
    cli::cli_alert_danger("Error {operation_name} file {.file {path}} in {.val {repo}}: {e$message}")
    return(NULL)
  })
}

#' Deploy a file to multiple GitHub repositories
#'
#' This function deploys a local file to multiple GitHub repositories. It can create new files
#' or update existing ones, and provides detailed status reporting for each operation.
#'
#' @param source_file Character string specifying the local file path to deploy
#' @param target_path Character string specifying the path in the repositories where the file should be placed
#' @param repos Data frame of repositories as returned by `repos()` function, with at least a `full_name` column
#' @param commit_message Character string with the commit message. Default automatically generates a message.
#' @param branch Character string specifying the branch name. Default is NULL (uses default branch).
#' @param create_if_missing Logical indicating whether to create the file if it doesn't exist. Default is TRUE.
#' @param dry_run Logical indicating whether to only simulate the changes without making actual commits. Default is FALSE.
#' @param quiet Logical; if TRUE, suppresses progress and status messages. Default is FALSE.
#'
#' @return 
#' Returns a `data.frame` with class `"file_deploy_result"` containing the following columns:
#' 
#' \describe{
#'   \item{repository}{Character, the full repository name (owner/repo)}
#'   \item{status}{Character, indicating the operation result with one of these values:
#'     "created", "updated", "unchanged", "skipped", "error", "would_create", "would_update"}
#'   \item{message}{Character, a description of the action taken or error encountered}
#' }
#' 
#' @seealso [`print.file_deploy_result()`] for a formatted summary of deployment results.
#'
#' @export
#'
#' @examplesIf interactive()
#' # Get list of repositories
#' repositories <- repos("my-organization")
#'
#' # Deploy a workflow file to all repositories
#' results <- file_deploy(
#'   source_file = "local/path/to/workflow.yml",
#'   target_path = ".github/workflows/ci.yml",
#'   repos = repositories
#' )
#' 
#' # Filter to see only successfully updated repositories
#' updated <- results[results$status == "updated", ]
#' 
#' # Check for any errors
#' errors <- results[results$status == "error", ]
file_deploy <- function(source_file, target_path, repos, 
                        commit_message = NULL, branch = NULL, 
                        create_if_missing = TRUE, dry_run = FALSE, quiet = FALSE) {
  
  # Read the local file
  if (!file.exists(source_file)) {
    cli::cli_abort("Source file does not exist: {.file {source_file}}")
  }
  
  file_content <- readChar(source_file, file.info(source_file)$size)
  
  if (is.null(commit_message)) {
    commit_message <- sprintf("Update %s via multideploy automated deployment", target_path)
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

#' Print method for `"file_deploy_result"` objects
#'
#' This method provides a formatted summary of file deployment results,
#' showing counts by status and details for any errors encountered.
#'
#' @param x An object of class `"file_deploy_result"` as returned by `file_deploy()`
#' @param ... Additional arguments passed to other print methods (not used)
#'
#' @return 
#' Invisibly returns the original input data frame unchanged.
#' 
#' Displays a formatted summary of deployment results to the console.
#'
#' @export
#'
#' @examplesIf interactive()
#' # Get list of repositories
#' repositories <- repos("my-organization")
#' 
#' # Deploy files
#' results <- file_deploy("local/file.R", "remote/file.R", repositories)
#' 
#' # Explicitly print the summary
#' print(results)
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