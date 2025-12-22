# paws.site.builder

Build tools for the Paws R SDK documentation site.

## Overview

The `paws.site.builder` package provides functions for building the Paws documentation website. It handles:

- Converting R documentation files (.Rd) to Markdown format
- Managing site assets, examples, and developer guides
- Building the site navigation structure
- Generating MkDocs configuration files

## Installation

You can install the development version from the local repository:

```r
# Install from local path
devtools::install("r-pkg/paws.site.builder")
```

## Main Functions

### Rd to Markdown Conversion

- `build_rd_to_md()`: Main function to convert Rd files to Markdown
- `rd_to_md()`: Convert a single Rd file to Markdown
- `build_long_rd()`: Build Rd documentation from roxygen2 comments

### Site Assets Management

- `build_all_assets()`: Build all site assets (wrapper function)
- `copy_site_assets()`: Copy assets from vendor directory
- `edit_readme()`: Edit README for site
- `edit_r_examples()`: Convert R examples to Markdown

### Site Structure

- `build_site_yaml()`: Generate mkdocs.yml configuration
- `make_hierarchy()`: Create site navigation hierarchy
- `reference_index()`: Create reference index page
- `get_developer_guide()`: Get developer guide files
- `get_examples()`: Get example files

### Utilities

- `log_info()`: Simple logging function
- `convert_name()`: Convert file names to title case

## Usage

### Building Rd Documentation to Markdown

```r
library(paws.site.builder)

# Build all Rd files and convert to Markdown
# Uses parallel processing with mirai for efficiency
build_rd_to_md(
  dir = "vendor/paws/paws/man",
  md_dir = "build/mkdocs/docs/docs",
  long_doc = "vendor/paws/paws",
  cores = parallel::detectCores()  # Uses all available cores by default
)
```

**Note**: The `build_rd_to_md()` function uses `mirai` for parallel processing. The package is automatically loaded on all worker daemons using `everywhere()`, ensuring all functions are available during parallel execution.

### Building Site Assets

```r
# Build all site assets
build_all_assets()

# Or run individual steps
copy_site_assets()
edit_readme()
edit_r_examples()
build_site_yaml()
```

## Original Code

The original scripts that this package was migrated from are:

- [build/rd2md.R](../../build/rd2md.R)
- [build/build_assests.R](../../build/build_assests.R)

These files remain in place for reference.

## License

Apache License (>= 2.0)
