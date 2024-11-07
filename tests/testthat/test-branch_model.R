
NAME <- "TEST"
test_model <- parsli::import_slim_model("../test.slim", name = NAME)
test_model_branched <- parsli::branch_model(test_model, branch_name = NAME)

test_that("branched model is equivalent to original", {
  expect_equal(test_model_branched, test_model)
})

test_that("branched model is separate from original", {
  test_model_branched$set_name("A different name")
  expect_equal(test_model$name, NAME)
})
