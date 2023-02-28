## Test demo function ----

test_that("Function demo() - Test for error", {
  
  expect_error(print_msg(c("Hello", "world")))
  expect_error(print_msg(iris), "Argument 'x' must be a character of length 1.")
  expect_error(print_msg(NULL), "Argument 'x' must be a character of length 1.")
  expect_error(print_msg(1), "Argument 'x' must be a character of length 1.")
})

test_that("Function demo() - Test for success", {
  
  expect_invisible(print_msg())
  
  x <- print_msg()
  expect_equal(x, "Hello world")
  
  x <- print_msg("Bonjour le monde")
  expect_equal(x, "Bonjour le monde")
  
  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
})
