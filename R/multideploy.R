#' List repositories for a user or organization
#'
#' @param owner Character string specifying the GitHub username or organization name
#' @param type Character string specifying the type of repositories to list: 
#'   "all", "owner", "public", "private", or "member". Default is "owner".
#' @param per_page Number of repositories to return per page. Default is 100.
#' @param max_pages Maximum number of pages to retrieve. Default is 10.
#' @param filter_regex Optional regular expression to filter repositories by name
#'
#' @return A data frame of repositories with their names and other metadata
#' @export
#'
#' @examples
#' \dontrun{
#' repositories <- repos("username")
#' repositories <- repos("orgname", type = "public", filter_regex = "^api-")
#' }
repos <- function(owner, type = "owner", per_page = 100, 
                  max_pages = 10, filter_regex = NULL, quiet = FALSE) {
  repo_list <- list()
  page <- 1
  
  if (!quiet) {
    cli::cli_progress_bar("Fetching repositories", total = max_pages)
  }
  
  while (page <= max_pages) {
    result <- gh::gh("GET /users/{username}/repos", 
                     username = owner,
                     type = type,
                     per_page = per_page,
                     page = page)
    
    if (!quiet) {
      cli::cli_progress_update()
    }
    
    if (length(result) == 0) {
      break
    }
    
    repo_list <- c(repo_list, result)
    if (length(result) < per_page) {
      break
    }
    
    page <- page + 1
  }
  
  if (!quiet) {
    cli::cli_progress_done()
  }
  
  # Convert to data frame
  repo_df <- data.frame(
    name = vapply(repo_list, function(x) x$name, character(1)),
    full_name = vapply(repo_list, function(x) x$full_name, character(1)),
    default_branch = vapply(repo_list, function(x) x$default_branch, character(1)),
    private = vapply(repo_list, function(x) x$private, logical(1)),
    stringsAsFactors = FALSE
  )
  
  total_repos <- nrow(repo_df)
  
  # Apply filter if provided
  if (!is.null(filter_regex)) {
    repo_df <- repo_df[grepl(filter_regex, repo_df$name), ]
    if (!quiet) {
      cli::cli_alert_info("Filtered {total_repos} repositories to {nrow(repo_df)} matching '{filter_regex}'")
    }
  } else if (!quiet) {
    cli::cli_alert_info("Found {total_repos} repositories for {.val {owner}}")
  }
  
  return(repo_df)
}

#' List organizations for the authenticated user
#'
#' @param per_page Number of organizations to return per page. Default is 100.
#' @param max_pages Maximum number of pages to retrieve. Default is 5.
#'
#' @return A data frame of organizations with their login names
#' @export
#'
#' @examples
#' \dontrun{
#' organizations <- orgs()
#' }
orgs <- function(per_page = 100, max_pages = 5, quiet = FALSE) {
  org_list <- list()
  page <- 1
  
  if (!quiet) {
    cli::cli_progress_bar("Fetching organizations", total = max_pages)
  }
  
  while (page <= max_pages) {
    result <- gh::gh("GET /user/orgs",
                     per_page = per_page,
                     page = page)
    
    cli::cli_progress_update()
    
    if (length(result) == 0) {
      break
    }
    
    org_list <- c(org_list, result)
    if (length(result) < per_page) {
      break
    }
    
    page <- page + 1
  }
  
  if (!quiet) {
    cli::cli_progress_done()
  }
  
  # Convert to data frame
  org_df <- data.frame(
    login = vapply(org_list, function(x) x$login, character(1)),
    url = vapply(org_list, function(x) x$url, character(1)),
    stringsAsFactors = FALSE
  )
  
  if (!quiet) {
    cli::cli_alert_info("Found {nrow(org_df)} organizations for authenticated user")
  }
  
  return(org_df)
}

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
      if (!quiet) {
        cli::cli_alert_warning("No files found in directory {.file {dir}}")
      }
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
      if (!quiet) {
        cli::cli_alert_warning("Local file does not exist: {.file {local_file}}")
      }
    }
  }
  
  if (!quiet) {
    cli::cli_alert_info("Created file mapping with {length(mapping)} files")
  }
  
  return(mapping)
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