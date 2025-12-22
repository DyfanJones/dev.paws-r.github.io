#' @importFrom fs path path_abs path_file file_delete dir_delete
#' @importFrom rmarkdown pandoc_convert
#' @importFrom parallel detectCores
#' @importFrom mirai daemons everywhere mirai_map
#' @importFrom tools Rd2HTML

#' @include rd_utils.R

#' @title Convert Rd file to Markdown
#'
#' @description Converts an R documentation file (.Rd) to Markdown format (.md) via HTML.
#' The function performs the following steps:
#' 1. Extracts the name from the Rd file
#' 2. Converts Rd to HTML using tools::Rd2HTML
#' 3. Converts HTML to Markdown using pandoc
#' 4. Adds URL links and formats code sections
#' 5. Converts HTML tables to definition lists
#'
#' @param rd_file Character string path to the Rd file
#' @param html_dir Character string path to temporary HTML directory
#' @param md_dir Character string path to output Markdown directory
#' @param clear_down Logical. If TRUE (default), deletes intermediate rd and html files
#'   after conversion. Set to FALSE to keep files for debugging or inspection.
#' @return NULL (invisibly)
#' @export
rd_to_md <- function(rd_file, html_dir, md_dir, clear_down = TRUE) {
  # get rd name and not use alias
  lines <- readLines(rd_file, n = 5)
  name <- lines[grep("\\\\name\\{", lines, perl = TRUE)]
  name <- gsub("\\\\name\\{|\\}", "", name)

  html_file <- path(html_dir, name, ext = "html")
  md_file <- path(md_dir, name, ext = "md")

  Rd2HTML(rd_file, html_file)

  # delete rd + html file
  on.exit(
    if (clear_down) file_delete(c(rd_file, html_file))
  )

  pandoc_convert(
    html_file,
    to = "markdown_strict",
    output = md_file
  )

  md <- readLines(md_file)
  # add url links
  if (!grepl("_", basename(md_file))) {
    idx <- grep('style=\"text-align: left;\">[a-z0-9_]+</td>', md)
    operator <- gsub("\\.md$", "", basename(md_file))
    for (j in idx) {
      md[[j]] <- find_and_replace(md[[j]], operator)
    }
  }

  md <- html_table_to_list(md)
  md <- wrap_r_code(md)
  writeLines(md, md_file)
}

#' Build Rd documentation and convert to Markdown
#'
#' Main function that orchestrates the entire Rd to Markdown conversion process.
#' This function:
#' 1. Creates necessary directories
#' 2. Builds Rd documentation
#' 3. Collects all Rd files to process
#' 4. Sets up parallel processing with mirai
#' 5. Converts all Rd files to Markdown in parallel
#'
#' @param dir Character string path to the directory containing Rd files.
#'   Default is "vendor/paws/paws/man"
#' @param long_doc Character string path to the package for building Rd docs.
#'   Default is "vendor/paws/paws"
#' @param md_dir Character string path to output Markdown directory.
#'   Default is "build/mkdocs/docs/docs"
#' @param html_dir Character string path to output HTML directory.
#'   Default is tempfile()
#' @param cores Integer number of cores to use for parallel processing.
#'   Default is parallel::detectCores()
#' @param clear_down Logical. If TRUE (default), deletes intermediate rd and html files
#'   after conversion. Set to FALSE to keep files for debugging or inspection.
#' @return NULL (invisibly)
#' @export
build_rd_to_md <- function(
  dir = "vendor/paws/paws/man",
  long_doc = "vendor/paws/paws",
  md_dir = "build/mkdocs/docs/docs",
  html_dir = tempfile("dir"),
  clear_down = TRUE,
  cores = detectCores()
) {
  # Create directories
  if (file.exists(md_dir)) {
    dir_delete(md_dir)
  }
  dir_create(c(md_dir, html_dir), recurse = TRUE)

  log_info("Build Rd docs")

  # create Rd docs
  build_long_rd(long_doc)

  log_info("Converting Rd to Markdown")

  files <- list.files(dir)
  # remove paws-package.Rd
  files <- files[files != "paws-package.Rd"]

  md_dir <- path_abs(md_dir)
  rd_files <- path_abs(file.path(dir, files))
  rd_files <- rd_files[path_file(rd_files) != "reexports.Rd"]
  addons <- c(
    "set_service_parameter.Rd",
    "paginate.Rd",
    "list_paginators.Rd",
    "paws_stream.Rd"
  )
  rd_files <- c(
    rd_files,
    path_abs(
      file.path(
        "vendor/paws",
        "paws.common",
        "man",
        addons
      )
    )
  )

  # Setup mirai
  log_info("Assigning %s cores.", cores)
  dd <- daemons(cores)

  # Clean up
  on.exit({
    daemons(0)
    if (clear_down) dir_delete(html_dir)
  })

  # Ensure dependencies are loaded on all daemons
  # This makes all exported functions and required packages available to worker processes
  everywhere({
    library(fs)
    library(rmarkdown)
    library(paws.site.builder)
  })

  mm <- mirai_map(
    rd_files,
    rd_to_md,
    .args = list(
      html_dir = html_dir,
      md_dir = md_dir,
      clear_down = clear_down
    )
  )[]
}
