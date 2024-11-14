
test_model <- parsli::import_slim_model("../test.slim", "Test")

test_that("import_slim_model returns EidosModel/EidosComposition/R6", {
  ALLOWED_CLASSES <- c("EidosModel", "EidosComposition", "R6")
  test_model_classes <- class(test_model)
  expect_equal(test_model_classes, expected = ALLOWED_CLASSES)
})

test_that("test_model contains correct number of lines", {
  MODEL_LINES <- 30
  expect_equal(test_model$line_count, expected = MODEL_LINES)
})

test_that("test_model contains correct number of blocks", {
  MODEL_BLOCKS <- 6
  expect_equal(test_model$block_count, expected = MODEL_BLOCKS)
})

test_that("test_model lines are EidosLines", {
  ALLOWED_CLASSES = c("EidosLine", "R6")
  for (line in test_model$lines)
  {
    if (! all(class(line) %in% ALLOWED_CLASSES))
    {
      fail()
    }
  }
  succeed()
})

test_that("test_model blocks are EidosBlocks", {
  ALLOWED_CLASSES = c("EidosBlock", "EidosComposition", "R6")
  for (line in test_model$blocks)
  {
    if (! all(class(line) %in% ALLOWED_CLASSES))
    {
      fail()
    }
  }
  succeed()
})
