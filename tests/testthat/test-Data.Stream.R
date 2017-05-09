context("Data.Stream.R")
described.class <- Data.Stream

# Currently not implemented
test_that("it should initialize", {
  expect_error(described.class$new(), 'Currently removed because of the many problems with rJava')
})
