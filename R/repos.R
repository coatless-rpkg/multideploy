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