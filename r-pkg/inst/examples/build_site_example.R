# Example script showing how to use paws.site.builder package
# This replaces the standalone scripts in build/rd2md.R and build/build_assests.R

library(paws.site.builder)

# ============================================================================
# Step 1: Build Rd documentation and convert to Markdown
# ============================================================================
# This step converts R documentation files (.Rd) to Markdown format
# It uses parallel processing for efficiency

log_info("Starting Rd to Markdown conversion")

build_rd_to_md(
  dir = "vendor/paws/paws/man",
  md_dir = "build/mkdocs/docs/docs",
  long_doc = "vendor/paws/paws",
  cores = parallel::detectCores()
)

log_info("Completed Rd to Markdown conversion")

# ============================================================================
# Step 2: Build site assets
# ============================================================================
# This step:
# - Copies assets from vendor directory (README, logo, examples, etc.)
# - Edits README for site-specific formatting
# - Converts R example files to Markdown
# - Builds the mkdocs.yml configuration file

log_info("Starting site assets build")

build_all_assets()

log_info("Completed site assets build")

# ============================================================================
# Alternative: Run individual asset steps
# ============================================================================
# If you need more control, you can run the asset building steps individually:

# copy_site_assets()   # Copy files from vendor
# edit_readme()        # Edit README for site
# edit_r_examples()    # Convert R examples to markdown
# build_site_yaml()    # Build mkdocs.yml configuration
