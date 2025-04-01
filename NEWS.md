# multideploy 0.1.0

## Initial Release

This is the first release of the `{multideploy}` package, providing tools for 
deploying file changes across multiple GitHub repositories.

### Features

#### Repository Management

- `repos()`: List repositories for a user or organization, with filtering options
- `orgs()`: List organizations for the authenticated user

#### File Operations

- `file_content()`: Retrieve contents of a file from a GitHub repository
- `file_update()`: Create or update a file in a GitHub repository
- `file_mapping()`: Create a mapping between local files and their target paths in repositories
- `file_deploy()`: Deploy a file to multiple GitHub repositories

#### Pull Request Operations

- `pr_create()`: Create a pull request for changes in multiple repositories

### Other Features

- Dry run mode `dry_run = TRUE` to preview changes without making actual commits
- Support for processing entire directories of files while preserving or flattening structure
- Detailed documentation with examples for all functions
