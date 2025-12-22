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
  # Simulate actual Rd2HTML output format with proper table structure
  # The function expects: first table tag to skip, then pairs of <table> and </table>
  html_lines <- c(
    "<table>",  # This gets skipped by [-1]
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
    "<table>",
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

  expect_true(any(grepl("<dl>", result)))
  expect_true(any(grepl("<dt>", result)))
  expect_true(any(grepl("<dd>", result)))
  expect_false(any(result == "REMOVE LINE"))
})
