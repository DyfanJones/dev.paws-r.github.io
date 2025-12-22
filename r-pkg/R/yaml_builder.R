#' @importFrom yaml yaml.load_file

#' @title Build site YAML configuration
#'
#' @description Generates the mkdocs.yml configuration file for the site by reading
#' the original template and adding dynamic content like references,
#' developer guide, and examples.
#'
#' @param template_file Character string path to the original mkdocs template.
#'   Default is "build/mkdocs.orig.yml"
#' @param output_file Character string path to the output mkdocs.yml file.
#'   Default is "build/mkdocs/mkdocs.yml"
#' @param docs_dir Character string path to docs directory.
#'   Default is "build/mkdocs/docs/docs"
#' @param developer_guide_dir Character string path to developer guide directory.
#'   Default is "build/mkdocs/docs/developer_guide"
#' @param examples_dir Character string path to examples directory.
#'   Default is "build/mkdocs/docs/examples"
#' @param version_file Character string path to DESCRIPTION file for version.
#'   Default is "vendor/paws/cran/paws/DESCRIPTION"
#' @param alias_file Character string path to AWS service alias YAML file.
#'   Default is "build/aws_service_alias.yml"
#' @param paws_dir Character string path to paws directory.
#'   Default is "vendor/paws/cran"
#' @return NULL (invisibly)
#' @export
build_site_yaml <- function(
  template_file = "build/mkdocs.orig.yml",
  output_file = "build/mkdocs/mkdocs.yml",
  docs_dir = "build/mkdocs/docs/docs",
  developer_guide_dir = "build/mkdocs/docs/developer_guide",
  examples_dir = "build/mkdocs/docs/examples",
  version_file = "vendor/paws/cran/paws/DESCRIPTION",
  alias_file = "build/aws_service_alias.yml",
  paws_dir = "vendor/paws/cran"
) {
  site_yaml <- org_yaml <- yaml.load_file(template_file)

  for (i in c("extra_css", "plugins")) {
    if (!is.null(org_yaml[[i]]) && !is.list(length(org_yaml[[i]]))) {
      site_yaml[[i]] <- as.list(site_yaml[[i]])
    }
  }

  site_yaml$site_name <- sprintf("paws: %s", get_version(version_file))

  # add references
  ref_idx <- which(
    vapply(site_yaml$nav, \(x) names(x) == "Reference", FUN.VALUE = logical(1))
  )
  site_yaml$nav[[ref_idx]]$Reference <- make_hierarchy(
    dir = docs_dir,
    alias_file = alias_file,
    paws_dir = paws_dir,
    output_file = file.path(docs_dir, "reference_index.md")
  )

  # add developer guide
  ref_idx <- which(
    vapply(
      site_yaml$nav,
      \(x) names(x) == "Developer Guide",
      FUN.VALUE = logical(1)
    )
  )
  site_yaml$nav[[ref_idx]][["Developer Guide"]] <- get_developer_guide(developer_guide_dir)

  # add examples
  ref_idx <- which(
    vapply(
      site_yaml$nav,
      \(x) names(x) == "Code Examples",
      FUN.VALUE = logical(1)
    )
  )
  site_yaml$nav[[ref_idx]][["Code Examples"]] <- get_examples(examples_dir)

  site_yaml <- yaml::as.yaml(site_yaml, indent.mapping.sequence = TRUE)
  site_yaml <- gsub("- '", "- ", site_yaml)
  site_yaml <- gsub(":\n          docs", ": docs", site_yaml)

  # tidy up file paths
  for (ext in c("md", "pdf")) {
    site_yaml <- gsub(
      sprintf("\\.%s'", ext),
      sprintf("\\.%s", ext),
      site_yaml
    )
  }
  writeLines(site_yaml, output_file, "")
}

#' @title Build all site assets
#'
#' @description Main wrapper function that orchestrates the entire site asset building process.
#' This includes copying assets, editing examples, and building the site YAML.
#'
#' @param vendor_dir Character string path to vendor directory.
#'   Default is "vendor/paws"
#' @param build_dir Character string path to build output directory.
#'   Default is "build/mkdocs/docs"
#' @param template_file Character string path to the original mkdocs template.
#'   Default is "build/mkdocs.orig.yml"
#' @param output_yaml Character string path to the output mkdocs.yml file.
#'   Default is "build/mkdocs/mkdocs.yml"
#' @param alias_file Character string path to AWS service alias YAML file.
#'   Default is "build/aws_service_alias.yml"
#' @param paws_dir Character string path to paws directory.
#'   Default is "vendor/paws/cran"
#' @return NULL (invisibly)
#' @export
build_all_assets <- function(
  vendor_dir = "vendor/paws",
  build_dir = "build/mkdocs/docs",
  template_file = "build/mkdocs.orig.yml",
  output_yaml = "build/mkdocs/mkdocs.yml",
  alias_file = "build/aws_service_alias.yml",
  paws_dir = "vendor/paws/cran"
) {
  copy_site_assets(
    vendor_dir = vendor_dir,
    build_dir = build_dir
  )
  edit_readme(file.path(build_dir, "README.md"))
  edit_r_examples(file.path(build_dir, "examples"))
  build_site_yaml(
    template_file = template_file,
    output_file = output_yaml,
    docs_dir = file.path(build_dir, "docs"),
    developer_guide_dir = file.path(build_dir, "developer_guide"),
    examples_dir = file.path(build_dir, "examples"),
    version_file = file.path(paws_dir, "paws/DESCRIPTION"),
    alias_file = alias_file,
    paws_dir = paws_dir
  )
}
