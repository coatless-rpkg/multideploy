

#' Create a pull request for changes in multiple repositories
#'
#' @param repos Data frame of repositories as returned by repos()
#' @param branch_name Character string with the name of the branch to create for the changes
#' @param base_branch Character string with the name of the base branch. Default is NULL (uses default branch).
#' @param title Character string with the PR title
#' @param body Character string with the PR description
#' @param create_branch Logical indicating whether to create the branch if it doesn't exist. Default is TRUE.
#' @param file_mapping List mapping local file paths to repository paths, as created by file_mapping()
#' @param dry_run Logical indicating whether to only simulate the changes. Default is FALSE.
#'
#' @return A data frame with the results of each PR creation
#' @export
#'
#' @examples
#' \dontrun{
#' repositories <- repos("myorg")
#' mapping <- file_mapping(
#'   "local/path/file1.R" = ".github/workflows/ci.yml",
#'   "local/path/file2.R" = "R/utils.R"
#' )
#' results <- pr_create(
#'   repos = repositories, 
#'   branch_name = "feature-branch", 
#'   title = "Update CI workflow", 
#'   body = "Standardizing CI workflow across repositories", 
#'   file_mapping = mapping
#' )
#' }
pr_create <- function(repos, branch_name, base_branch = NULL, 
                      title, body, create_branch = TRUE, 
                      file_mapping, dry_run = FALSE, quiet = FALSE) {
  
  # Initialize results data frame
  results <- data.frame(
    repository = repos$full_name,
    pr_url = NA_character_,
    status = NA_character_,
    message = NA_character_,
    stringsAsFactors = FALSE
  )
  
  if (!quiet) {
    cli::cli_h1("Pull Request Creation")
    cli::cli_alert_info("Branch name: {.val {branch_name}}")
    cli::cli_alert_info("Files to change: {.val {length(file_mapping)}}")
    cli::cli_alert_info("Repositories: {.val {nrow(repos)}}")
    if (dry_run) {
      cli::cli_alert_warning("DRY RUN MODE - No changes will be made")
    }
  }
  
  # Validate file mapping
  for (local_file in names(file_mapping)) {
    if (!file.exists(local_file)) {
      if (!quiet) {
        cli::cli_alert_warning("Local file {.file {local_file}} doesn't exist")
      }
    }
  }
  
  if (!quiet) {
    cli::cli_progress_bar("Processing repositories", total = nrow(repos))
  }
  
  for (i in seq_len(nrow(repos))) {
    repo <- repos$full_name[i]
    
    cli::cli_progress_update()
    
    tryCatch({
      # Get the default branch if base_branch is NULL
      if (is.null(base_branch)) {
        base_branch <- repos$default_branch[i]
      }
      
      # Check if branch exists or create it
      branch_exists <- FALSE
      
      existing_branches <- gh::gh("GET /repos/{repo}/branches", repo = repo)
      branch_exists <- any(vapply(existing_branches, function(x) x$name == branch_name, logical(1)))
      
      if (!branch_exists) {
        if (!create_branch) {
          results$status[i] <- "skipped"
          results$message[i] <- "Branch doesn't exist and create_branch is FALSE"
          next
        }
        
        # Get base branch reference
        base_ref <- gh::gh("GET /repos/{repo}/git/ref/heads/{branch}", 
                           repo = repo, 
                           branch = base_branch)
        
        # Create new branch if not in dry run mode
        if (!dry_run) {
          gh::gh("POST /repos/{repo}/git/refs", 
                 repo = repo,
                 ref = paste0("refs/heads/", branch_name),
                 sha = base_ref$object$sha)
          
          if (!quiet) {
            cli::cli_alert_success("Created branch {.val {branch_name}} in {.val {repo}}")
          }
        } else if (!quiet) {
          cli::cli_alert("Would create branch {.val {branch_name}} in {.val {repo}}")
        }
      }
      
      # Process each file in the mapping
      for (local_file in names(file_mapping)) {
        repo_path <- file_mapping[[local_file]]
        
        if (!file.exists(local_file)) {
          results$message[i] <- paste(results$message[i], 
                                      sprintf("Warning: Local file %s does not exist", local_file),
                                      sep = "; ")
          next
        }
        
        file_content <- readChar(local_file, file.info(local_file)$size)
        
        # Get the current file content and SHA (if it exists)
        current <- file_content(repo, repo_path, ref = base_branch)
        
        if (!dry_run) {
          if (is.null(current)) {
            # Create new file
            file_update(repo, repo_path, file_content, 
                        sprintf("Add %s", repo_path), 
                        branch = branch_name,
                        quiet = quiet)
          } else {
            # Update existing file
            file_update(repo, repo_path, file_content, 
                        sprintf("Update %s", repo_path), 
                        branch = branch_name, 
                        sha = current$sha,
                        quiet = quiet)
          }
        } else if (!quiet) {
          if (is.null(current)) {
            cli::cli_alert("Would create {.file {repo_path}} in {.val {repo}}")
          } else {
            cli::cli_alert("Would update {.file {repo_path}} in {.val {repo}}")
          }
        }
      }
      
      # Create pull request if not in dry run mode
      if (!dry_run) {
        pr <- gh::gh("POST /repos/{repo}/pulls", 
                     repo = repo,
                     title = title,
                     body = body,
                     head = branch_name,
                     base = base_branch)
        
        results$pr_url[i] <- pr$html_url
        results$status[i] <- "created"
        results$message[i] <- "Pull request created successfully"
        
        if (!quiet) {
          cli::cli_alert_success("Created PR in {.val {repo}}: {.url {pr$html_url}}")
        }
      } else {
        results$status[i] <- "would_create"
        results$message[i] <- "Would create pull request (dry run)"
        
        if (!quiet) {
          cli::cli_alert("Would create PR in {.val {repo}}")
        }
      }
      
    }, error = function(e) {
      results$status[i] <- "error"
      results$message[i] <- paste("Error:", e$message)
      
      if (!quiet) {
        cli::cli_alert_danger("Error in {.val {repo}}: {e$message}")
      }
    })
  }
  
  if (!quiet) {
    cli::cli_progress_done()
  }
  
  # Set class for custom print method
  class(results) <- c("pr_create_result", class(results))
  
  # Print summary if not quiet
  if (!quiet) {
    print(results)
  }
  
  return(results)
}

#' Print method for pr_create_result objects
#'
#' @param x The pr_create_result object to print
#' @param ... Additional arguments to pass to print methods
#'
#' @return Invisibly returns the input data frame
#' @export
#'
#' @examples
#' \dontrun{
#' results <- pr_create(repos, "branch", "title", "body", file_mapping)
#' print(results)
#' }
print.pr_create_result <- function(x, ...) {
  # Summary
  status_counts <- table(x$status)
  cli::cli_h2("Pull Request Summary")
  
  # Print status counts with appropriate styling
  for (status in names(status_counts)) {
    count <- status_counts[[status]]
    
    # Choose appropriate alert type based on status
    alert_fn <- switch(status,
                       created = cli::cli_alert_success,
                       would_create = cli::cli_alert_warning,
                       skipped = cli::cli_alert_warning,
                       error = cli::cli_alert_danger,
                       cli::cli_alert_info)
    
    # Call the alert function
    alert_fn("{status}: {count} repositories")
  }
  
  # Show created PRs
  created_prs <- x[!is.na(x$pr_url), ]
  if (nrow(created_prs) > 0) {
    cli::cli_h3("Created Pull Requests")
    for (i in seq_len(nrow(created_prs))) {
      cli::cli_bullets(c("*" = "{.val {created_prs$repository[i]}}: {.url {created_prs$pr_url[i]}}"))
    }
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