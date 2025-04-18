% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pr.R
\name{pr_create}
\alias{pr_create}
\title{Create a pull request for changes in multiple repositories}
\usage{
pr_create(
  repos,
  branch_name,
  base_branch = NULL,
  title,
  body,
  create_branch = TRUE,
  file_mapping,
  dry_run = FALSE,
  quiet = FALSE
)
}
\arguments{
\item{repos}{Data frame of repositories as returned by \code{repos()}, with at least
columns for \code{full_name} and \code{default_branch}}

\item{branch_name}{Character string with the name of the branch to create for the changes}

\item{base_branch}{Character string with the name of the base branch. Default is NULL (uses default branch).}

\item{title}{Character string with the PR title}

\item{body}{Character string with the PR description}

\item{create_branch}{Logical indicating whether to create the branch if it doesn't exist. Default is TRUE.}

\item{file_mapping}{List mapping local file paths to repository paths, as created by \code{file_mapping()}}

\item{dry_run}{Logical indicating whether to only simulate the changes. Default is FALSE.}

\item{quiet}{Logical; if TRUE, suppresses progress and status messages. Default is FALSE.}
}
\value{
Returns a \code{data.frame} with class \code{"pr_create_result"} containing the following columns:

\describe{
\item{repository}{Character, the full repository name (owner/repo)}
\item{pr_url}{Character, the URL of the created pull request, or NA if no PR was created}
\item{status}{Character, indicating the operation result: "created", "would_create",
"skipped", or "error"}
\item{message}{Character, a description of the action taken or error encountered}
}
}
\description{
This function creates pull requests across multiple GitHub repositories, applying
the same set of file changes to each repository. It can create new branches as needed,
add or update files, and then open pull requests.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Get repositories and create file mapping
repositories <- repos("my-organization")
mapping <- file_mapping(
  "local/path/file1.R" = ".github/workflows/ci.yml",
  "local/path/file2.R" = "R/utils.R"
)

# Create pull requests in all repositories
results <- pr_create(
  repos = repositories, 
  branch_name = "feature-branch", 
  title = "Update CI workflow", 
  body = "Standardizing CI workflow across repositories", 
  file_mapping = mapping
)

# Simulate without making actual changes
dry_run_results <- pr_create(
  repos = repositories, 
  branch_name = "feature-branch", 
  title = "Update documentation", 
  body = "Updating documentation with new examples", 
  file_mapping = mapping,
  dry_run = TRUE
)

# Only create PRs in repositories where the branch already exists
existing_branch_results <- pr_create(
  repos = repositories, 
  branch_name = "existing-branch", 
  title = "Fix existing branch", 
  body = "Apply fixes to existing branch", 
  file_mapping = mapping,
  create_branch = FALSE
)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=print.pr_create_result]{print.pr_create_result()}} to display the results in a formatted way.
}
