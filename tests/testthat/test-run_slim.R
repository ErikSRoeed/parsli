
SEED <- 123
TEST_REPS <- 10
expected_test_output <- readRDS("../test.rds")
test_model <- parsli::import_slim_model("../test.slim", "Test")
test_output <- parsli::run_slim(test_model, seed = SEED)
replicated_test_output <- parsli::run_slim(test_model, reps = TEST_REPS)

test_that("run_slim returns expected output", {
  expect_equal(test_output$script, expected = expected_test_output$script)
  expect_equal(test_output$seed, expected = expected_test_output$seed)
  expect_equal(test_output$constants, expected = expected_test_output$constants)
  expect_equal(test_output$replicate_runs, expected = expected_test_output$replicate_runs)
  expect_equal(test_output$slim_outputs, expected = expected_test_output$slim_outputs)
})

test_that("replicated simulations are captured as output", {
  expect_length(replicated_test_output$slim_outputs, TEST_REPS)
})
