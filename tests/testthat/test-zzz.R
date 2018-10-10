context("Global.R")
context(" Implements / extends the correct functions")
test_that("it should define the expit function correctly", {
  expect_true(identical(deparse(expit), deparse(plogis)))
})
test_that("it should define the logit function correctly", {
  expect_true(identical(deparse(logit), deparse(qlogis)))
})

context(" is.a")
test_that("it should throw if the provided class is not a string", {
 a <- data.frame(a=c(1,2,3))
 expect_error(is.a(a, data.frame), 'cannot coerce type \'closure\' to vector of type \'character\'')
})

test_that("it should not throw if the provided class is a string", {
 a <- data.frame(a=c(1,2,3))
 expect_error(is.a(a, 'hoi'), NA)
 expect_false(is.a(a, 'hoi'))
})

test_that("it should check if the class is correct", {
 a <- data.frame(a=c(1,2,3))
 expect_true(is.a(a, class(a)))
})


context(" get_file_location")
glob_file_name <- 'testfile'
glob_extension <- 'pdf'
glob_dir <- 'tmptmp'
glob_subdir <- 'subsub'
test_that("it should create a correct file name without subdir and dir", {
  result <- get_file_location(glob_file_name, glob_extension, create_dirs = FALSE)
  dir <- file.path('tmp', glob_file_name)
  expected <- paste(dir, '.', glob_extension, sep='')
  expect_equal(result, expected)
})

test_that("it should create a correct filename with subdir", {
  result <- get_file_location(glob_file_name, glob_extension, 
                              subdir = glob_subdir,
                              create_dirs = FALSE)
  dir <- file.path('tmp', glob_subdir, glob_file_name)
  expected <- paste(dir, '.', glob_extension, sep='')
  expect_equal(result, expected)
})

test_that("it should create a correct filename with date dir inside", {
  result <- get_file_location(glob_file_name, glob_extension, 
                              subdir = glob_subdir,
                              add_date_to_dir = TRUE,
                              create_dirs = FALSE)
  date <- format(Sys.time(), "%y%m%d")

  ## Note! This test is not perfect, and it will fail when you run it at 24:00...
  expected <- paste('[^0-9]*', date, '.*', sep='')
  expect_true(grepl(expected, result))
})

test_that("it should create a correct filename with a date dir and subdir", {
  result <- get_file_location(glob_file_name, glob_extension, 
                              subdir = glob_subdir,
                              dir = glob_dir,
                              create_dirs = FALSE)

  dir <- file.path(glob_dir, glob_subdir, glob_file_name)
  expected <- paste(dir, '.', glob_extension, sep='')
  expect_true(grepl(expected, result))
})
