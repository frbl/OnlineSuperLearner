context("DataCache.R")
described.class <- DataCache

context(" update_cache") 
test_that("it should add to the existing cache if the estimator is offline", {
  subject <- described.class$new(online = FALSE)
  expect_null(subject$get_data_cache)
  added_data <- data.table(a = 123)
  expect_true(subject$update_cache(added_data))
  expect_equal(subject$get_data_cache, added_data)

  ## Add it again to actually test if it gets appended
  expect_true(subject$update_cache(added_data))
  expect_equal(subject$get_data_cache, rbindlist(list(added_data, added_data)))
})

test_that("it should return the new data if the estimator is online", {
  subject <- described.class$new(online = TRUE)
  added_data <- data.table(a = 123)
  expect_false(subject$update_cache(added_data))
  expect_equal(subject$get_data_cache, added_data)

  ## Add it again to actually test if it gets appended
  subject$update_cache(added_data)
  expect_equal(subject$get_data_cache, added_data)
})

test_that("it should be online by default", {
  subject <- described.class$new()
  added_data <- data.table(a = 123)
  for (i in 1:10) {
    expect_false(subject$update_cache(added_data))
  }
  
  expect_equal(subject$get_data_cache, added_data)
})

context(" is_online") 
test_that("it should return true if the cache is online", {
  subject <- described.class$new(online = TRUE)
  expect_true(subject$is_online)
})

test_that("it should return false if the cache is offline", {
  subject <- described.class$new(online = FALSE)
  expect_false(subject$is_online)
})
