context("DataCache.R")
described.class <- DataCache

context(" initialize")
#==========================================================
test_that("it should initialize", {
  expect_error(described.class$new, NA)
  expect_error(described.class$new(online = TRUE), NA)
})

test_that("it should initialze the online status", {
  subject <- described.class$new(online = TRUE)
  expect_true(subject$is_online)

  subject <- described.class$new(online = FALSE)
  expect_false(subject$is_online)
})

test_that("it should be online by default", {
  subject <- described.class$new()
  expect_true(subject$is_online)
})

context(" update_cache") 
#==========================================================
test_that("it should add to the existing cache if the estimator is offline", {
  subject <- described.class$new(online = FALSE)
  expect_null(subject$get_data_cache)
  added_data <- data.table(a = 123)

  ## Add the data once
  result <- subject$update_cache(added_data)
  expect_true(result)

  resulting_cache <- subject$get_data_cache
  expect_is(resulting_cache, 'data.table')
  expect_equal(resulting_cache, added_data)

  ## Add it again to actually test if it gets appended
  result <- subject$update_cache(added_data)
  expect_true(result)

  resulting_cache <- subject$get_data_cache
  expect_is(resulting_cache, 'data.table')
  expect_equal(resulting_cache, rbindlist(list(added_data, added_data)))
})

test_that("it should return the new data if the estimator is online", {
  subject <- described.class$new(online = TRUE)
  added_data <- data.table(a = 123)

  ## Add the data once
  result <- subject$update_cache(added_data)
  expect_false(result)

  resulting_cache <- subject$get_data_cache
  expect_is(resulting_cache, 'data.table')
  expect_equal(resulting_cache, added_data)

  ## Add it again to actually test that it does not gets appended
  result <- subject$update_cache(added_data)
  expect_false(result)

  resulting_cache <- subject$get_data_cache
  expect_is(resulting_cache, 'data.table')
  expect_equal(resulting_cache, added_data)
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
#==========================================================
test_that("it should return true if the cache is online", {
  subject <- described.class$new(online = TRUE)
  expect_true(subject$is_online)
})

test_that("it should return false if the cache is offline", {
  subject <- described.class$new(online = FALSE)
  expect_false(subject$is_online)
})

context(" get_data_cache")
#==========================================================
test_that("it should return the data cache", {
  subject <- described.class$new()
  added_data <- data.table(a = 123)

  ## Add the data once
  subject$update_cache(added_data)

  result <- subject$get_data_cache
  expect_equal(result, added_data)
})

test_that("it should return NULL if no cache is available", {
  subject <- described.class$new()
  result <- subject$get_data_cache
  expect_null(result)
})

