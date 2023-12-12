## Test coords_to_address function ----

test_that("Function coords_to_address() - Test for error", {
  
  expect_error(coords_to_address(), 
               "Argument 'coords' is required", 
               fixed = TRUE)
  
  expect_error(coords_to_address(NULL), 
               "Argument 'coords' cannot be NULL", 
               fixed = TRUE)
  
  expect_error(coords_to_address(iris), 
               "Argument 'coords' must be numeric", 
               fixed = TRUE)
  
  expect_error(coords_to_address("character"), 
               "Argument 'coords' must be numeric", 
               fixed = TRUE)
  
  expect_error(coords_to_address(0.00), 
               "Argument 'coords' must be numeric of length 2", 
               fixed = TRUE)
  
  expect_error(coords_to_address(c(0.00, 0.00, 0.00)), 
               "Argument 'coords' must be numeric of length 2", 
               fixed = TRUE)
  
  expect_error(coords_to_address(c(180.00, 0.00)), 
               "Latitude must be the first element of 'coords'", 
               fixed = TRUE)
})


test_that("Function coords_to_address() - Test for success", {
  
  ## Good address ----
  
  expect_silent({
    x <- coords_to_address(c(29.75894, -95.3677))
  })
  
  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
  
  expect_null(names(x))

  expect_equal(x, paste0("One Shell Plaza, 910, Louisiana Street, Houston, ", 
                         "Harris County, Texas, 77002, United States"))
  
  
  ## Bad address (Atlantic Ocean) ----
  
  expect_silent({
    x <- coords_to_address(c(-40.429864, 21.471329))
  })
  
  expect_equal(class(x), "NULL")
  expect_equal(length(x), 0L)
  
  expect_null(names(x))
  
  expect_null(x)
})
