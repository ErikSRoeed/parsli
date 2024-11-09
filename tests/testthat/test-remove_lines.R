
base_model <- parsli::import_slim_model("../test.slim")
actual_script <- base_model$get_lines_as_character()

test_that("out of bounds indices raise error", {
  out_of_bounds <- length(actual_script) + 1
  expect_error(parsli::remove_lines(base_model, out_of_bounds))
  expect_error(parsli::remove_lines(base_model, -out_of_bounds))
})

test_that("all within bounds indices raise no error when .force = TRUE", {
  for (legal_index in 1 : length(actual_script))
  {
    modified_model <- parsli::branch_model(base_model, "Modified")
    expect_no_error(parsli::remove_lines(modified_model, legal_index, .force = TRUE))
  }
})

test_that("trying to remove a callback line raises an error when .force = FALSE", {
  callback_lines <- which(sapply(base_model$lines, function(line) line$is_callback))

  for (callback_line in callback_lines)
  {
    modified_model <- parsli::branch_model(base_model, "Modified")
    expect_error(parsli::remove_lines(modified_model, callback_line))
  }
})

test_that("correct lines are removed", {
  n_lines <- length(actual_script)
  scrambled_line_numbers <- sample(1 : n_lines, n_lines)

  for (i in 1 : n_lines)
  {
    remove_lines <- scrambled_line_numbers[1 : i]

    modified_model <- parsli::branch_model(base_model, "Modified")
    modified_model |> parsli::remove_lines(remove_lines)
    modified_script <- actual_script[-remove_lines]

    expect_equal(modified_model$get_lines_as_character(), modified_script)
  }
})
