
test_model <- parsli::import_slim_model("../test.slim", name = "Test")

test_that("replace_eidos_pattern changes the script when intended", {
  modified_model <- parsli::branch_model(test_model, branch_name = "Branch")
  modified_model |> parsli::replace_eidos_pattern(pattern = "m1", replacement = "m2")

  expect_failure(
    expect_equal(
      test_model$get_lines_as_character(),
      modified_model$get_lines_as_character()
    )
  )
})

test_that("replace_eidos_pattern does not change the script if replacement and substitution are equal", {
  modified_model <- parsli::branch_model(test_model, branch_name = "Branch")
  modified_model |> parsli::replace_eidos_pattern(pattern = "m1", replacement = "m1")

  expect_equal(
    test_model$get_lines_as_character(),
    modified_model$get_lines_as_character()
  )
})

test_that("replace_eidos_pattern only changes targeted block(s) if in_blocks", {
  TARGET_BLOCK <- 3

  modified_model <- parsli::branch_model(test_model, branch_name = "Branch")
  modified_model |> parsli::replace_eidos_pattern(pattern = "p1", replacement = "p2", in_blocks = TARGET_BLOCK)

  expect_failure(
    expect_equal(
      test_model$blocks[[TARGET_BLOCK]]$get_lines_as_character(),
      modified_model$blocks[[TARGET_BLOCK]]$get_lines_as_character()
    )
  )

  test_modified <- parsli::branch_model(test_model, branch_name = "Test Modified")
  test_modified |> parsli::remove_blocks(TARGET_BLOCK)
  modified_model |> parsli::remove_blocks(TARGET_BLOCK)

  expect_equal(
    test_modified$get_lines_as_character(),
    modified_model$get_lines_as_character()
  )
})
