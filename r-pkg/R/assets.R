#' @importFrom fs file_copy file_delete path dir_delete dir_create dir_copy dir_ls
#' @importFrom heck to_title_case

#' @title Copy site assets from vendor
#'
#' @description Copies assets like README, logo, examples, and developer guide files
#' from the vendor directory to the site build directory.
#'
#' @param vendor_dir Character string path to vendor directory.
#'   Default is "vendor/paws"
#' @param build_dir Character string path to build output directory.
#'   Default is "build/mkdocs/docs"
#' @return NULL (invisibly)
#' @export
copy_site_assets <- function(
  vendor_dir = "vendor/paws",
  build_dir = "build/mkdocs/docs"
) {
  log_info("Build site assets")

  # copy assets from vendor
  file_copy(
    file.path(vendor_dir, "README.md"),
    build_dir,
    overwrite = TRUE
  )
  file_copy(
    file.path(vendor_dir, "docs/logo.png"),
    build_dir,
    overwrite = TRUE
  )

  # create site directory structure
  dirs <- path(build_dir, c("examples", "developer_guide", "img"))
  if (all(file.exists(dirs))) {
    dir_delete(dirs)
  }
  dir_create(dirs, recurse = TRUE)

  # copy assets examples from vendor
  dir_copy(
    file.path(vendor_dir, "examples"),
    file.path(build_dir, "examples"),
    overwrite = TRUE
  )

  vendor_docs <- file.path(vendor_dir, "docs")
  developer_guide_files <- list.files(vendor_docs)[
    !grepl("\\.png$|\\.gif$", list.files(vendor_docs))
  ]
  # copy articles from vendor
  for (f in developer_guide_files) {
    file_copy(
      file.path(vendor_docs, f),
      file.path(build_dir, "developer_guide", f),
      overwrite = TRUE
    )
  }

  # copy articles from vendor
  for (f in c("CODE_OF_CONDUCT.md", "DEVELOPER_GUIDE.md")) {
    file_copy(
      file.path(vendor_dir, f),
      file.path(build_dir, "developer_guide", f),
      overwrite = TRUE
    )
  }

  # copy read assets
  for (f in c("code_completion.gif")) {
    file_copy(
      file.path(vendor_docs, f),
      file.path(build_dir, "img", f),
      overwrite = TRUE
    )
  }
}

#' @title Edit README for site
#'
#' @description Edits the README.md file to fix logo images, documentation links,
#' and example links for the site.
#'
#' @param file Character string path to README.md file.
#'   Default is "build/mkdocs/docs/README.md"
#' @return NULL (invisibly)
#' @export
edit_readme <- function(file = "build/mkdocs/docs/README.md") {
  readme <- readLines(file)
  # fix logo image
  idx <- grep('<img src="docs/logo.png" align="right" height="150" />', readme)
  readme[idx] <- gsub(
    '<img src="docs/logo.png" align="right" height="150" />',
    '<img src= "logo.png" style="float:right;height:150px;width:auto" />',
    readme[idx]
  )

  # fix docs links
  idx <- grepl(r"{\[.*\]\(docs/.*\)|\[.*\]\(developer_guide/docs/.*\)}", readme)
  readme[idx] <- gsub(r"{\[Logo\]\(docs/logo.png\)}", "Logo", readme[idx])
  readme[idx] <- gsub(
    r"{!\[\]\(docs/code_completion.gif\)}",
    r"{!\[\]\(img/code_completion\.gif\)}",
    readme[idx]
  )
  readme[idx] <- gsub(
    "docs/developer_guide|docs",
    "developer_guide",
    readme[idx]
  )

  # fix examples links
  idx <- grepl(r"{\[.*\]\(examples/.*\)}", readme)
  readme[idx] <- gsub(r"{\.R\)}", r"{\.md\)}", readme[idx])

  writeLines(readme, file)
}

#' @title Edit R example files
#'
#' @description Converts R example files to Markdown format by wrapping them in code fences
#' and changing the file extension from .R to .md.
#'
#' @param dir Character string path to examples directory.
#'   Default is "build/mkdocs/docs/examples"
#' @return NULL (invisibly)
#' @export
edit_r_examples <- function(dir = "build/mkdocs/docs/examples") {
  files <- dir_ls(dir)
  r_files <- files[grepl("\\.R$", files)]

  for (r_file in r_files) {
    r_file_edit <- readLines(r_file)
    r_file_edit <- c("```r", r_file_edit, "```")
    writeLines(r_file_edit, gsub("R$", "md", r_file))
  }
  file_delete(r_files)
}

#' @title Convert file names to title case
#'
#' @param file_names Character vector of file names
#' @return Character vector of converted names
#' @export
convert_name <- function(file_names) {
  file_names <- gsub("\\..*$", "", file_names)
  return(to_title_case(file_names))
}
