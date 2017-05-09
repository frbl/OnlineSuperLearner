context("Data.Base.R")
described.class <- Data.Base
context(" initialize")
test_that("it should initialize", {
 subject <- described.class$new() 
 expect_true(is.a(subject, 'Data.Base'))
})

context(" getNext")
test_that("it should throw an error, this function needs to be inherited", {
 subject <- described.class$new() 
 expect_error(subject$getNext(), 'This method needs to be inherited in a subclass')
})

context(" getNextN")
test_that("it should throw an error, this function needs to be inherited, when providing an n", {
 subject <- described.class$new() 
 expect_error(subject$getNextN(n = 1), 'This method needs to be inherited in a subclass')
})

test_that("it should throw an error, this function needs to be inherited, when not an n", {
 subject <- described.class$new() 
 expect_error(subject$getNextN(), 'This method needs to be inherited in a subclass')
})
