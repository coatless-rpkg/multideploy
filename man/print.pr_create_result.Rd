% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pr.R
\name{print.pr_create_result}
\alias{print.pr_create_result}
\title{Print method for pr_create_result objects}
\usage{
\method{print}{pr_create_result}(x, ...)
}
\arguments{
\item{x}{An object of class "pr_create_result" as returned by \code{pr_create()}}

\item{...}{Additional arguments passed to other print methods (not used)}
}
\value{
Invisibly returns the original input data frame (x) unchanged, allowing for
chained operations. The function's primary purpose is displaying a formatted
summary to the console, including:
\itemize{
\item Counts of PRs by status (created, would_create, skipped, error)
\item List of successfully created PRs with clickable URLs
\item Details about any errors encountered during the process
}
}
\description{
This method provides a formatted summary of pull request creation results,
showing counts by status and details for created PRs and any errors encountered.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Create PRs
results <- pr_create(
  repos = repos("my-organization"),
  branch_name = "feature-branch",
  title = "Update configuration",
  body = "Standardize configuration across repos",
  file_mapping = file_mapping("config.yml" = ".github/config.yml")
)

print(results)  # Explicitly print the summary
\dontshow{\}) # examplesIf}
}
