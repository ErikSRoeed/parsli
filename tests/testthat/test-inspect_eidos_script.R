
test_model <- parsli::import_slim_model("../test.slim", "Test")

test_that("inspect_eidos_script raises no error", {
  expect_no_error(capture.output(parsli::inspect_eidos_script(test_model)))
})
