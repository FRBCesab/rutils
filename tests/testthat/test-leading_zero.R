## Test leading_zero function ----

test_that("Function leading_zero() - Test for error", {
  
  expect_error(leading_zero(), 
               "Argument 'x' is required", 
               fixed = TRUE)
  
  expect_error(leading_zero(iris), 
               "Argument 'x' must be a numeric or a character vector", 
               fixed = TRUE)
  
  expect_error(leading_zero(c(TRUE, FALSE, TRUE)), 
               "Argument 'x' must be a numeric or a character vector", 
               fixed = TRUE)
  
  expect_error(leading_zero(c(1, NA, 2)), 
               "Argument 'x' cannot contain any NA values", 
               fixed = TRUE)
  
  expect_error(leading_zero(c("1", "10", "I123")), 
               "Unable to convert 'x' to a numeric", 
               fixed = TRUE)
})


test_that("Function leading_zero() - Test for success", {
  
  expect_silent({
    x <- leading_zero(1:10)
  })
  
  expect_equal(class(x), "character")
  expect_equal(length(x), length(1:10))
  expect_equal(x[1:1], "01")
})
