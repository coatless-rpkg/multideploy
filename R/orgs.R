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