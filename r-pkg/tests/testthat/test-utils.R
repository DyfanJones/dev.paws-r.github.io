test_that("convert_name converts to title case", {
  expect_equal(convert_name("test_file.md"), "Test File")
  expect_equal(convert_name("my_example.R"), "My Example")
})

test_that("find_and_replace adds links correctly", {
  string <- '<td style="text-align: left;">test_function</td>'
  result <- find_and_replace(string, "s3")
  expect_true(grepl('<a href="../s3_test_function/">', result))
})

test_that("html_table_to_list converts tables to definition lists", {
  # Simulate actual Rd2HTML output format with proper HTML structure
  # The function expects: first table (header), then argument tables
  html_lines <- c(
    '<table style="width: 100%;"><tr><td>header</td></tr></table>',  # Header table (gets skipped by [-1])
    "Some text",
    "<table>",
    "<tr><td><code>param1</code></td>",
    "<td>",
    "<p>Description 1</p>",
    "</td></tr>",
    "</table>",
    "More text",
    '<table role="presentation">',
    "<tr><td><code>param2</code></td>",
    "<td>",
    "<p>Description 2</p>",
    "</td></tr>",
    "</table>"
  )

  result <- html_table_to_list(html_lines)

  expect_true(any(grepl("<dl>", result)))
  expect_true(any(grepl("<dt>", result)))
  expect_true(any(grepl("<dd>", result)))
  expect_true(any(grepl("<dt><code>param1</code></dt>", result)))
  expect_true(any(grepl("<dt><code>param2</code></dt>", result)))
  expect_false(any(result == "REMOVE LINE"))

  # <tr> tags should be removed from argument tables but remain in header table
  dl_indices <- which(grepl("<dl>", result))
  if (length(dl_indices) > 0) {
    after_dl <- result[dl_indices[1]:length(result)]
    expect_false(any(grepl("<tr>", after_dl)), "Should remove tr tags from argument tables")
  }
})
