
TARGET_BLOCK <- 6
OUT_OF_BOUNDS_BLOCK <- 1000

test_model <- parsli::import_slim_model("../test.slim", name = "Test")
original_block_count <- test_model$block_count
test_model |> parsli::duplicate_block(TARGET_BLOCK, TARGET_BLOCK)

test_that("out of bounds block indices raises error", {
  expect_error(parsli::duplicate_block(test_model, OUT_OF_BOUNDS_BLOCK, 0))
  expect_error(parsli::duplicate_block(test_model, -OUT_OF_BOUNDS_BLOCK, 0))
})

test_that("out of bounds after_index raises error", {
  expect_error(parsli::duplicate_block(test_model, TARGET_BLOCK, OUT_OF_BOUNDS_BLOCK))
  expect_error(parsli::duplicate_block(test_model, TARGET_BLOCK, -OUT_OF_BOUNDS_BLOCK))
})

test_that("within bounds block indices and after_index raises no error", {
  ALLOWED_BLOCK_INDICES <- 1 : 6
  ALLOWED_AFTER_INDICES <- 0 : 6

  for (allowed_index in ALLOWED_BLOCK_INDICES)
  {
    new_test_model <- parsli::import_slim_model("../test.slim")
    expect_no_error(parsli::duplicate_block(new_test_model, allowed_index, 0))
  }

  for (allowed_index in ALLOWED_AFTER_INDICES)
  {
    new_test_model <- parsli::import_slim_model("../test.slim")
    expect_no_error(parsli::duplicate_block(new_test_model, TARGET_BLOCK, allowed_index))
  }

})

test_that("block is duplicated", {
  expect_equal(test_model$block_count, original_block_count + 1)
})

test_that("duplicated block is identical", {
  original_block <- test_model$blocks[[TARGET_BLOCK]]
  duplicated_block <- test_model$blocks[[TARGET_BLOCK + 1]]
  expect_equal(original_block, duplicated_block)
})

test_that("no side effects on other blocks", {
  unchanged_model <- parsli::import_slim_model("../test.slim", name = "Test")
  expect_equal(unchanged_model$blocks, test_model$blocks[-TARGET_BLOCK])
})

test_that("duplicated block is moved to correct index if within bounds", {
  LOWEST_IN_BOUNDS_ARGUMENT <- 0
  HIGHEST_IN_BOUNDS_ARGUMENT <- 6
  for (after_index in LOWEST_IN_BOUNDS_ARGUMENT : HIGHEST_IN_BOUNDS_ARGUMENT)
  {
    new_test_model <- parsli::import_slim_model("../test.slim")
    original_block <- new_test_model$blocks[[TARGET_BLOCK]]

    new_test_model |> parsli::duplicate_block(TARGET_BLOCK, after_index)
    putative_duplicate_block <- new_test_model$blocks[[after_index + 1]]

    expect_equal(original_block, putative_duplicate_block)
  }
})

test_that("raise error if index negatively out of bounds", {
  NEGATIVE_OUT_OF_BOUNDS_INDEX <- -100
  new_test_model <- parsli::import_slim_model("../test.slim")
  expect_error(parsli::duplicate_block(new_test_model, TARGET_BLOCK, NEGATIVE_OUT_OF_BOUNDS_INDEX))
})

test_that("raise error if index positively out of bounds", {
  POSITIVE_OUT_OF_BOUNDS_INDEX <- 100
  new_test_model <- parsli::import_slim_model("../test.slim")
  expect_error(parsli::duplicate_block(new_test_model, TARGET_BLOCK, POSITIVE_OUT_OF_BOUNDS_INDEX))
})
