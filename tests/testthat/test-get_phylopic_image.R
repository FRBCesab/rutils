## Test get_phylopic_image function ----

test_that("Function get_phylopic_image() - Test for error", {
  
  expect_error(get_phylopic_image(), 
               "You must provide an UUID (argument 'uuid')", fixed = TRUE)
  
  expect_error(get_phylopic_image(iris), 
               "Argument 'uuid' must be a character of length 1", fixed = TRUE)
  expect_error(get_phylopic_image(NULL), 
               "Argument 'uuid' must be a character of length 1", fixed = TRUE)
  expect_error(get_phylopic_image(1L), 
               "Argument 'uuid' must be a character of length 1", fixed = TRUE)
  expect_error(get_phylopic_image(LETTERS), 
               "Argument 'uuid' must be a character of length 1", fixed = TRUE)
  
  expect_error(get_phylopic_image("string", size = iris), 
               paste0("Argument 'size' must be a vector ('character' or ", 
                      "'integer') of length 1"), fixed = TRUE)
  expect_error(get_phylopic_image("string", size = NULL), 
               paste0("Argument 'size' must be a vector ('character' or ", 
                      "'integer') of length 1"), fixed = TRUE)
  expect_error(get_phylopic_image("string", size = LETTERS), 
               paste0("Argument 'size' must be a vector ('character' or ", 
                      "'integer') of length 1"), fixed = TRUE)

  expect_error(get_phylopic_image("string", size = 512), 
               "The resource cannot be found", fixed = TRUE)
  
  uuid <- "8cad2b22-30d3-4cbd-86a3-a6d2d004b201"
  
  expect_error(get_phylopic_image(uuid, size = 600), 
               "Size not found. Available sizes are: '1536', '1024', '512'.", 
               fixed = TRUE)
})

test_that("Function get_phylopic_image() - Test for success", {

  uuid <- "8cad2b22-30d3-4cbd-86a3-a6d2d004b201"
  
  expect_silent({
    img <- get_phylopic_image(uuid, size = 512)
  })
  
  expect_equal(class(img), "array")
  expect_equal(dim(img), c(399, 512, 4))
  
})