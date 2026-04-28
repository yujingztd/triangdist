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
