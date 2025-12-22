test_that("log_info outputs correctly formatted message", {
  # Capture output
  output <- capture.output(log_info("Test message"))

  # Check that output contains expected elements
  expect_true(grepl("INFO", output))
  expect_true(grepl("Test message", output))
  expect_true(grepl("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", output))
})
