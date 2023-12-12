## Test address_to_coords function ----

test_that("Function address_to_coords() - Test for error", {
  
  expect_error(address_to_coords(), 
               "Argument 'address' is required", 
               fixed = TRUE)
  
  expect_error(address_to_coords(NULL), 
               "Argument 'address' cannot be NULL", 
               fixed = TRUE)
  
  expect_error(address_to_coords(iris), 
               "Argument 'address' must be character", 
               fixed = TRUE)
  
  expect_error(address_to_coords(1L), 
               "Argument 'address' must be character", 
               fixed = TRUE)
  
  expect_error(address_to_coords(letters), 
               "Argument 'address' must be character of length 1", 
               fixed = TRUE)
})


test_that("Function address_to_coords() - Test for success", {
  
  ## Good address ----
  
  expect_silent({
    x <- address_to_coords("Houston, TX, USA")
  })
  
  expect_equal(class(x), "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(ncol(x), 3L)
  
  expect_true(colnames(x)[1] == "lon")
  expect_true(colnames(x)[2] == "lat")
  expect_true(colnames(x)[3] == "name")
  
  expect_equal(class(x[ , 1]), "numeric")
  expect_equal(class(x[ , 2]), "numeric")
  expect_equal(class(x[ , 3]), "character")
  
  expect_equal(x[1, "lon"], -95.3677, tolerance = 0.0001)
  expect_equal(x[1, "lat"],  29.75894, tolerance = 0.0001)
  expect_equal(x[1, "name"],  "Houston, Harris County, Texas, United States")
  
  
  ## Bad address ----
  
  expect_silent({
    x <- address_to_coords("Totoland")
  })
  
  expect_equal(class(x), "data.frame")
  expect_equal(nrow(x), 0L)
  expect_equal(ncol(x), 3L)
  
  expect_true(colnames(x)[1] == "lon")
  expect_true(colnames(x)[2] == "lat")
  expect_true(colnames(x)[3] == "name")
  
  expect_equal(class(x[ , 1]), "numeric")
  expect_equal(class(x[ , 2]), "numeric")
  expect_equal(class(x[ , 3]), "character")
})
