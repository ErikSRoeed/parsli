
OUT_OF_BOUNDS <- 100
base_model <- parsli::import_slim_model("../test.slim", name = "Base")

test_that("out of bounds indices raise error", {
  expect_error(parsli::remove_blocks(base_model, OUT_OF_BOUNDS))
  expect_error(parsli::remove_blocks(base_model, -OUT_OF_BOUNDS))
})

test_that("a block is removed when a legal index is passed", {
  legal_indices <- 1 : base_model$block_count

  for (index in legal_indices)
  {
    modified_model <- base_model |> parsli::branch_model("Modified")
    modified_model |> parsli::remove_blocks(index)
    expect_equal(modified_model$block_count, base_model$block_count - 1)
  }
})

test_that("only correct block(s) are removed across legal indices", {
  scrambled_indices <- sample(1 : base_model$block_count, base_model$block_count)

  for (i in 1 : length(scrambled_indices))
  {
    i_indices <- scrambled_indices[1 : i]

    modified_model <- base_model |> parsli::branch_model("Modified")
    modified_model |> parsli::remove_blocks(i_indices)

    correct_remaining_blocks <- base_model$blocks[-i_indices]
    expect_equal(modified_model$blocks, expect = correct_remaining_blocks)
  }
})
