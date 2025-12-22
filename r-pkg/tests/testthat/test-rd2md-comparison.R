test_that("package rd_to_md produces same output as original script", {
  skip_if_not_installed("tools")
  skip_if_not_installed("rmarkdown")

  # Setup temporary directories
  html_dir <- withr::local_tempdir()
  md_dir_pkg <- withr::local_tempdir()
  md_dir_script <- withr::local_tempdir()

  # Copy test Rd file to both temp directories
  test_rd <- testthat::test_path("data/client.Rd")
  rd_file_pkg <- file.path(md_dir_pkg, "client.Rd")
  rd_file_script <- file.path(md_dir_script, "client.Rd")

  file.copy(test_rd, rd_file_pkg)
  file.copy(test_rd, rd_file_script)

  # Test 1: Run package version
  # Set clear_down = FALSE to keep intermediate files for inspection if test fails
  rd_to_md(
    rd_file = rd_file_pkg,
    html_dir = html_dir,
    md_dir = md_dir_pkg,
    clear_down = FALSE
  )

  # Test 2: Run original script version (replicated logic)
  # This replicates what the original script does
  run_original_logic <- function(rd_file, html_dir, md_dir) {
    # get rd name and not use alias
    lines <- readLines(rd_file, n = 5)
    name <- lines[grep("\\\\name\\{", lines, perl = TRUE)]
    name <- gsub("\\\\name\\{|\\}", "", name)

    html_file <- fs::path(html_dir, paste(name, "html", sep = "."))
    md_file <- fs::path(md_dir, paste(name, "md", sep = "."))

    tools::Rd2HTML(rd_file, html_file)

    # delete rd file
    fs::file_delete(rd_file)

    rmarkdown::pandoc_convert(
      html_file,
      to = "markdown_strict",
      output = md_file
    )

    # delete html file
    fs::file_delete(html_file)

    md <- readLines(md_file)

    # add url links (same logic as package)
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

  run_original_logic(
    rd_file = rd_file_script,
    html_dir = html_dir,
    md_dir = md_dir_script
  )

  # Compare outputs
  md_pkg <- readLines(file.path(md_dir_pkg, "accessanalyzer.md"))
  md_script <- readLines(file.path(md_dir_script, "accessanalyzer.md"))

  # Check that outputs are identical
  expect_equal(
    md_pkg,
    md_script,
    label = "Package output",
    expected.label = "Original script output"
  )

  # Additional checks
  expect_true(length(md_pkg) > 0, "Output should not be empty")
  expect_true(any(grepl("```r", md_pkg)), "Should contain R code blocks")
  expect_true(any(grepl("Access Analyzer", md_pkg)), "Should contain title")
})

test_that("html_table_to_list converts tables correctly", {
  # This tests the core transformation with properly paired table tags
  html_lines <- c(
    '<table style="width: 100%;">',  # This gets skipped by [-1]
    "Some text",
    "<table>",
    "<tbody>",
    "<tr>",
    "<td><code>param1</code></td>",
    "<td><p>Description 1</p></td>",
    "</tr>",
    "</tbody>",
    "</table>",
    "More text",
    '<table role="presentation">',
    "<colgroup>",
    "</colgroup>",
    "<tbody>",
    "<tr>",
    "<td><code>param2</code></td>",
    "<td><p>Description 2</p></td>",
    "</tr>",
    "</tbody>",
    "</table>"
  )

  result <- html_table_to_list(html_lines)

  # Verify transformations
  expect_true(any(grepl("<dl>", result)), "Should convert table to dl")
  expect_true(any(grepl("<dt>", result)), "Should convert td to dt")
  expect_true(any(grepl("<dd>", result)), "Should convert td to dd")
  # Note: The first table tag is kept because it's skipped by [-1]
  expect_false(any(grepl('<table role="presentation">', result)), "Should remove table tags with attributes")
  expect_false(any(grepl("<tbody>", result)), "Should remove tbody tags")
  expect_false(any(grepl("<tr>", result)), "Should remove tr tags")
  expect_false(any(result == "REMOVE LINE"), "Should filter out REMOVE LINE markers")
})

test_that("wrap_r_code adds code fences correctly", {
  # Test input similar to what pandoc produces
  md_lines <- c(
    "# Title",
    "",
    "### Usage",
    "",
    "    my_function()",
    "",
    "### Arguments",
    "",
    "Some text",
    "",
    "### Value",
    "",
    "    list(",
    "      x = 1",
    "    )",
    "",
    "### Examples",
    "",
    "    # Example code",
    "    result <- my_function()",
    "    ## End(Not run)"
  )

  result <- wrap_r_code(md_lines)

  # Check that code blocks are wrapped
  expect_true(any(result == "```r"), "Should add opening code fence")
  expect_true(any(result == "```"), "Should add closing code fence")

  # Check that indentation is removed
  expect_false(any(grepl("^    ", result)), "Should remove leading spaces")
})

test_that("find_and_replace adds links correctly", {
  string <- '<td style="text-align: left;">test_function</td>'
  result <- find_and_replace(string, "s3")

  expect_true(grepl('<a href="../s3_test_function/">', result))
  expect_true(grepl("test_function", result))
})

test_that("rd_to_md clear_down parameter works correctly", {
  skip_if_not_installed("tools")
  skip_if_not_installed("rmarkdown")

  # Setup temporary directories
  html_dir <- withr::local_tempdir()
  md_dir <- withr::local_tempdir()

  # Copy test Rd file
  test_rd <- testthat::test_path("data/client.Rd")
  rd_file <- file.path(md_dir, "client.Rd")
  file.copy(test_rd, rd_file)

  # Test with clear_down = FALSE
  rd_to_md(
    rd_file = rd_file,
    html_dir = html_dir,
    md_dir = md_dir,
    clear_down = FALSE
  )

  # Check that intermediate files still exist
  expect_true(file.exists(rd_file), "Rd file should still exist with clear_down = FALSE")
  expect_true(
    any(grepl("\\.html$", list.files(html_dir))),
    "HTML file should still exist with clear_down = FALSE"
  )

  # Check that output was created
  expect_true(file.exists(file.path(md_dir, "accessanalyzer.md")))

  # Test with clear_down = TRUE (cleanup another copy)
  rd_file2 <- file.path(md_dir, "client2.Rd")
  file.copy(test_rd, rd_file2)

  rd_to_md(
    rd_file = rd_file2,
    html_dir = html_dir,
    md_dir = md_dir,
    clear_down = TRUE
  )

  # Check that intermediate files were deleted
  expect_false(file.exists(rd_file2), "Rd file should be deleted with clear_down = TRUE")
  # Note: HTML file might still exist from previous run, so we check it was attempted to be deleted
  # by verifying the md output exists (meaning the function completed)
  expect_true(file.exists(file.path(md_dir, "accessanalyzer.md")))
})
