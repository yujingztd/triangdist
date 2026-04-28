# TEST dtriang
test_that("dtriang raises errors when parameters are not valid", {
  # 'min' greater than 'max'
  expect_error(dtriang(x = 1, min = 3, max = 1, mode = 2))

  # mode out of range
  expect_error(dtriang(x = 1, min = 0, max = 3, mode = 4))
  expect_error(dtriang(x = 1, min = 0, max = 3, mode = -1))
})

test_that("dtriang calculates correctly the density", {
  # Values out of the triangle (must return 0)
  expect_equal(dtriang(-1, min = 0, max = 3, mode = 1), 0)
  expect_equal(dtriang(4, min = 0, max = 3, mode = 1), 0)

  # Valid value (ex. x = 0.5)
  expect_equal(dtriang(0.5, min = 0, max = 3, mode = 1), 1/3)

  # Valid value (ex. x = 1)
  expect_equal(dtriang(1, min = 0, max = 3, mode = 1), 2/3)

  # Valid value (ex. x = 2)
  expect_equal(dtriang(2, min = 0, max = 3, mode = 1), 1/3)
})



# TESTS ptriang
test_that("ptriang validates parameters and calculates probabilities", {
  # Errors
  expect_error(ptriang(1, min = 3, max = 1, mode = 2))
  expect_error(ptriang(1, min = 0, max = 3, mode = 4))

  # Operations
  expect_equal(ptriang(-1, min = 0, max = 3, mode = 1), 0) # Antes del min
  expect_equal(ptriang(4, min = 0, max = 3, mode = 1), 1)  # Después del max
  expect_equal(ptriang(1, min = 0, max = 3, mode = 1), 1/3) # Exactamente en la moda

  # Probability (ex. x = 2)
  expect_equal(ptriang(2, min = 0, max = 3, mode = 1), 5/6)
})
