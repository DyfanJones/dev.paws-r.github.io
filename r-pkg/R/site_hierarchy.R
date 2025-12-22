#' @importFrom fs dir_ls path file_exists file_delete
#' @importFrom yaml read_yaml

#' @title Create reference index page
#'
#' @description Generates a reference index page listing all available AWS services
#' organized by package.
#'
#' @param paws_dir Character string path to paws directory.
#'   Default is "vendor/paws/cran"
#' @param alias_file Character string path to AWS service alias YAML file.
#'   Default is "build/aws_service_alias.yml"
#' @param output_file Character string path to output reference index file.
#'   Default is "build/mkdocs/docs/docs/reference_index.md"
#' @return Character string path to the reference index file
#' @export
reference_index <- function(
  paws_dir = "vendor/paws/cran",
  alias_file = "build/aws_service_alias.yml",
  output_file = "build/mkdocs/docs/docs/reference_index.md"
) {
  paws_desc <- path(paws_dir, "paws/DESCRIPTION")
  lines <- readLines(paws_desc)
  pkgs <- lines[grepl("paws\\.[a-z\\.]", lines, perl = TRUE)]
  paws_pkg <- trimws(gsub("\\([^)]*\\).*", "", pkgs))
  paws_pkg <- paws_pkg[paws_pkg != "paws.common"]

  reference <- vector("list", length(paws_pkg))
  names(reference) <- paws_pkg

  override <- as.data.frame(
    do.call(rbind, read_yaml(alias_file))
  )
  for (pkg in paws_pkg) {
    file_list <- gsub(
      "\\.Rd$",
      "\\.md",
      basename(dir_ls(file.path(paws_dir, pkg, "man")))
    )
    lvl <- gsub("_.*|\\.md$", "", file_list)
    ref <- sub("[a-zA-Z0-9]+_", "", file_list, perl = TRUE)
    ref <- gsub("\\.md$", "", ref)
    ref <- ref[lvl == ref]
    ref <- ref[ref != "reexports"]

    found <- override$service %in% ref
    ref_name <- as.character(
      ifelse(
        ref %in% override$service,
        sprintf("%s (%s)", override$name[found], ref),
        sprintf("%s (%s)", convert_name(ref), ref)
      )
    )

    reference[[pkg]] <- paste(
      sprintf('- <a href="../%s/"> %s </a>', ref, ref_name),
      collapse = "\n"
    )
  }
  names(reference) <- sprintf("## %s", names(reference))
  reference <- paste(names(reference), reference, sep = "\n")
  # Delete file if exists
  if (file_exists(output_file)) {
    file_delete(output_file)
  }
  writeLines(
    c("# Available Services", reference),
    output_file
  )
  return("docs/reference_index.md")
}

#' @title Create site hierarchy structure
#'
#' @description Creates the hierarchical structure for the site navigation,
#' organizing documentation by service and operation.
#'
#' @param dir Character string path to docs directory.
#'   Default is "build/mkdocs/docs/docs"
#' @param alias_file Character string path to AWS service alias YAML file.
#'   Default is "build/aws_service_alias.yml"
#' @param paws_dir Character string path to paws directory.
#'   Default is "vendor/paws/cran"
#' @param output_file Character string path to output reference index file.
#'   Default is "build/mkdocs/docs/docs/reference_index.md"
#' @return Named list representing the site hierarchy
#' @export
make_hierarchy <- function(
  dir = "build/mkdocs/docs/docs",
  alias_file = "build/aws_service_alias.yml",
  paws_dir = "vendor/paws/cran",
  output_file = "build/mkdocs/docs/docs/reference_index.md"
) {
  addons <- c(
    "set_service_parameter.md",
    "paginate.md",
    "list_paginators.md",
    "paws_stream.md"
  )
  hierarchy <- list.files(dir)
  hierarchy <- hierarchy[!(hierarchy %in% addons)]

  lvl <- gsub("_.*|\\.md$", "", hierarchy)
  ref <- sub("[a-zA-Z0-9]+_", "", hierarchy, perl = TRUE)
  ref <- gsub("\\.md$", "", ref)

  ref[lvl == ref] <- "Client"

  hierarchy <- sprintf("%s: docs/%s", convert_name(ref), hierarchy)
  hierarchy <- split(hierarchy, lvl)

  # order hierarchy
  for (j in seq_along(hierarchy)) {
    idx <- grep("^Client:", hierarchy[[j]], perl = TRUE)
    hierarchy[[j]] <- c(hierarchy[[j]][idx], sort(hierarchy[[j]][-idx]))
  }
  override <- as.data.frame(
    do.call(rbind, read_yaml(alias_file))
  )
  found <- override$service %in% names(hierarchy)
  names(hierarchy) <- as.character(
    ifelse(
      names(hierarchy) %in% override$service,
      sprintf("%s (%s)", override$name[found], override$service[found]),
      sprintf("%s (%s)", convert_name(names(hierarchy)), names(hierarchy))
    )
  )
  addons <- setNames(sprintf("docs/%s", addons), convert_name(addons))

  # group paginators
  pag_n <- grepl("paginat", addons)
  addons <- c(addons[!pag_n], list("Paws Paginators" = addons[pag_n]))

  hierarchy <- c(
    "Available Services" = reference_index(
      paws_dir = paws_dir,
      alias_file = alias_file,
      output_file = output_file
    ),
    addons,
    hierarchy
  )
  return(hierarchy)
}

#' @title Get developer guide files
#'
#' @description Retrieves and formats developer guide documentation files.
#'
#' @param dir Character string path to developer_guide directory.
#'   Default is "build/mkdocs/docs/developer_guide"
#' @return Named list of developer guide files
#' @export
get_developer_guide <- function(dir = "build/mkdocs/docs/developer_guide") {
  developer_guide <- sort(
    basename(dir_ls(dir, type = "file")),
    decreasing = TRUE
  )
  developer_guide <- sprintf(
    "%s: developer_guide/%s",
    convert_name(developer_guide),
    developer_guide
  )
  return(as.list(developer_guide))
}

#' @title Get example files
#'
#' @description Retrieves and formats example files.
#'
#' @param dir Character string path to examples directory.
#'   Default is "build/mkdocs/docs/examples"
#' @return Named list of example files
#' @export
get_examples <- function(dir = "build/mkdocs/docs/examples") {
  example <- basename(dir_ls(dir, type = "file"))
  example <- sprintf("%s: examples/%s", convert_name(example), example)
  return(as.list(example))
}

#' @title Get package version
#'
#' @description Extracts the version number from the DESCRIPTION file.
#'
#' @param dir Character string path to DESCRIPTION file.
#'   Default is "vendor/paws/cran/paws/DESCRIPTION"
#' @return Character string version number
#' @export
get_version <- function(dir = "vendor/paws/cran/paws/DESCRIPTION") {
  desc <- readLines(dir)
  version <- desc[grepl("Version:*.[0-9]+\\.[0-9]+\\.[0-9]+", desc)]
  pattern <- "[0-9]+\\.[0-9]+\\.[0-9]+"
  m <- regexpr(pattern, version)
  regmatches(version, m)
}
