
MOVE_FROM <- 1
MOVE_AFTER <- 6
OUT_OF_BOUNDS <- 100

test_model <- parsli::import_slim_model("../test.slim", name = "Test")
test_model |> parsli::move_block(MOVE_FROM, MOVE_AFTER)

test_that("out of bounds block indices raises error", {
  expect_error(parsli::move_block(test_model, OUT_OF_BOUNDS, 0))
  expect_error(parsli::move_block(test_model, -OUT_OF_BOUNDS, 0))
})

test_that("out of bounds after_index raises error", {
  expect_error(parsli::move_block(test_model, MOVE_FROM, OUT_OF_BOUNDS_BLOCK))
  expect_error(parsli::move_block(test_model, MOVE_FROM, -OUT_OF_BOUNDS_BLOCK))
})

test_that("within bounds block indices and after_index raises no error", {
  ALLOWED_BLOCK_INDICES <- 1 : 6
  ALLOWED_AFTER_INDICES <- 0 : 6

  for (allowed_index in ALLOWED_BLOCK_INDICES)
  {
    new_test_model <- parsli::import_slim_model("../test.slim")
    expect_no_error(parsli::move_block(new_test_model, allowed_index, 0))
  }

  for (allowed_index in ALLOWED_AFTER_INDICES)
  {
    new_test_model <- parsli::import_slim_model("../test.slim")
    expect_no_error(parsli::move_block(new_test_model, MOVE_FROM, allowed_index))
  }
})

test_that("block is moved and no blocks are added", {
  new_test_model <- parsli::import_slim_model("../test.slim", name = "Pristine")
  block_in_original_location <- new_test_model$blocks[[MOVE_FROM]]
  block_in_derived_location <- test_model$blocks[[MOVE_AFTER]]
  expect_equal(block_in_original_location, block_in_derived_location)
  expect_equal(new_test_model$block_count, test_model$block_count)
})
