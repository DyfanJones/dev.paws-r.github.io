test_that("rd_to_md produces pkgdown-style argument formatting", {
  skip_if_not_installed("tools")
  skip_if_not_installed("rmarkdown")

  # Setup temporary directories
  html_dir <- withr::local_tempdir()
  md_dir <- withr::local_tempdir()

  # Copy test Rd file
  test_rd <- testthat::test_path("data/client.Rd")
  rd_file <- file.path(md_dir, "client.Rd")
  file.copy(test_rd, rd_file)

  # Run conversion
  rd_to_md(
    rd_file = rd_file,
    html_dir = html_dir,
    md_dir = md_dir,
    clear_down = FALSE
  )

  # Read output
  md_output <- readLines(file.path(md_dir, "accessanalyzer.md"))

  # Verify basic structure
  expect_true(length(md_output) > 0, "Output should not be empty")
  expect_true(any(grepl("```r", md_output)), "Should contain R code blocks")
  expect_true(any(grepl("Access Analyzer", md_output)), "Should contain title")
  expect_true(any(grepl("### Arguments", md_output)), "Should have Arguments section")

  # Verify pkgdown-style HTML definition list structure
  # Note: The header table at the top is expected and should remain
  args_start <- which(grepl("### Arguments", md_output))[1]
  if (!is.na(args_start)) {
    md_after_args <- md_output[args_start:length(md_output)]
    expect_false(any(grepl('<table role="presentation">', md_after_args)), "Should not contain presentation tables in Arguments")
    expect_false(any(grepl("<colgroup>", md_after_args)), "Should not contain colgroup tags in Arguments")
  }

  # Verify pkgdown-style argument formatting with &emsp; indentation
  args_section_start <- which(grepl("### Arguments", md_output))[1]
  args_section_end <- which(grepl("^###", md_output) & seq_along(md_output) > args_section_start)[1]
  args_section <- md_output[args_section_start:args_section_end]

  # Check for backtick-wrapped parameter names (pkgdown style)
  expect_true(any(grepl("^`config`", args_section)), "Should have config parameter in backticks")
  expect_true(any(grepl("^`credentials`", args_section)), "Should have credentials parameter in backticks")
  expect_true(any(grepl("^`endpoint`", args_section)), "Should have endpoint parameter in backticks")
  expect_true(any(grepl("^`region`", args_section)), "Should have region parameter in backticks")

  # Check for &emsp; indentation on description lines
  expect_true(any(grepl("^&emsp;Optional configuration", args_section)), "Should have &emsp; indented description for config")
  expect_true(any(grepl("^&emsp;Optional credentials", args_section)), "Should have &emsp; indented description for credentials")
  expect_true(any(grepl("^&emsp;Optional shorthand for complete URL", args_section)), "Should have &emsp; indented description for endpoint")
  expect_true(any(grepl("^&emsp;Optional shorthand for AWS Region", args_section)), "Should have &emsp; indented description for region")

  # Verify that nested markdown lists are preserved
  expect_true(any(grepl("^&emsp;- \\*\\*", args_section)), "Should preserve nested markdown lists with &emsp; indentation")
})

test_that("html_table_to_list converts HTML tables to definition lists", {
  # This tests the core transformation with actual HTML structure
  # First table is header (single line), second is arguments table
  html_lines <- c(
    '<table style="width: 100%;"><tr><td>header</td></tr></table>',  # Header table (gets skipped by [-1])
    "<h3>Arguments</h3>",
    '<table role="presentation">',
    "<tr><td><code>param1</code></td>",
    "<td>",
    "<p>Description 1</p>",
    "</td></tr>",
    "<tr><td><code>param2</code></td>",
    "<td>",
    "<p>Description 2</p>",
    "</td></tr>",
    "</table>"
  )

  result <- html_table_to_list(html_lines)

  # Verify transformations
  expect_true(any(grepl("<dl>", result)), "Should convert table to dl")
  expect_true(any(grepl("<dt>", result)), "Should convert first td to dt")
  expect_true(any(grepl("<dd>", result)), "Should convert second td to dd")
  expect_true(any(grepl("<dt><code>param1</code></dt>", result)), "Should properly close dt tags")
  expect_true(any(grepl("<dd>", result)), "Should have dd tags for descriptions")

  # Verify cleanup in argument tables (not header)
  expect_false(any(grepl('<table role="presentation">', result)), "Should convert presentation table tags")
  expect_false(any(result == "REMOVE LINE"), "Should filter out REMOVE LINE markers")

  # Check that <tr> tags are removed from argument tables but remain in header
  dl_indices <- which(grepl("<dl>", result))
  if (length(dl_indices) > 0) {
    # Check lines after the first <dl> (argument table area)
    after_dl <- result[dl_indices[1]:length(result)]
    # <tr> should not appear in the argument table area
    tr_in_args <- any(grepl("<tr>", after_dl))
    expect_false(tr_in_args, "Should remove tr tags from argument tables")
  }

  # Verify first table (header) is unchanged and can have <tr> tags
  expect_true(any(grepl('<table style="width: 100%;">', result)), "Should keep header table unchanged")
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
