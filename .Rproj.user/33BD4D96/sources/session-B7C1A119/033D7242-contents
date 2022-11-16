test_that('test contingency', {
  x <- c(0, 1, 0, 1, 0, 1, 0, 0)
  y <- c(0, 1, 0, 1, 0, 1, 1, 1)
  cTable <- contingencyTable2groups(x, y)
  expect_equal(cTable[1,1], 5)
  expect_equal(cTable[1,2], 3)
  expect_equal(cTable[2,1], 3)
  expect_equal(cTable[2,2], 5)
})
