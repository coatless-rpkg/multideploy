#' List organizations for the authenticated user
#'
#' This function retrieves all organizations associated with the currently authenticated
#' GitHub user, with options to control pagination.
#'
#' @param per_page Number of organizations to return per page. Default is 100.
#' @param max_pages Maximum number of pages to retrieve. Default is 5.
#' @param quiet Logical; if TRUE, suppresses progress and status messages. Default is FALSE.
#'
#' @return 
#' Returns a `data.frame` of organizations with the following columns:
#' 
#' \describe{
#'   \item{login}{Character, the organization's username/login name}
#'   \item{url}{Character, the API URL for the organization}
#' }
#' 
#' The `data.frame` is ordered as returned by the GitHub API (typically alphabetically).
#'
#' @export
#'
#' @examplesIf interactive()
#' # Get all organizations for the authenticated user
#' my_orgs <- orgs()
#' 
#' # Retrieve silently without progress messages
#' my_orgs <- orgs(quiet = TRUE)
#' 
#' # Extract just the organization names
#' org_names <- orgs()$login
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
    
    if (!quiet) {
      cli::cli_progress_update()
    }
    
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