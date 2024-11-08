
TARGET_BLOCK <- 4 # 1000 late()
NEW_CALLBACK <- "500 late()"
LINE_IN_SCRIPT <- 28
WRONG_CALLBACK <- "abc()"

test_model <- parsli::import_slim_model("../test.slim", name = "Test")
test_model |> parsli::change_block_callback(TARGET_BLOCK, NEW_CALLBACK)

test_that("callback changed at level of EidosBlock", {
  block_callback <- test_model$blocks[[TARGET_BLOCK]]$callback
  expect_equal(block_callback, expect = NEW_CALLBACK)
})

test_that("callback changed at level of EidosLine", {
  callback_line_number <- test_model$blocks[[TARGET_BLOCK]]$callback_line_number
  callback_line <- test_model$blocks[[TARGET_BLOCK]]$lines[[callback_line_number]]
  line_callback <- callback_line$callback
  expect_equal(line_callback, NEW_CALLBACK)
})

test_that("callback changes in script", {
  script_as_character <- test_model$get_lines_as_character()
  callback_line_as_character <- script_as_character[[LINE_IN_SCRIPT]]
  callback_as_character <- substr(callback_line_as_character, start = 1, stop = nchar(NEW_CALLBACK))
  expect_equal(callback_as_character, expect = NEW_CALLBACK)
})

test_that("non-callback value to new_callback raises error", {
  # Callbacks should be only on formats "1 : 100 late()" or "20 early()", etc.
  expect_no_error(parsli::change_block_callback(test_model, TARGET_BLOCK, NEW_CALLBACK))
  expect_error(parsli::change_block_callback(test_model, TARGET_BLOCK, WRONG_CALLBACK))
})
