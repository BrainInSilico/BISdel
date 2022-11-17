library(psych)

test_that('nb levels', {
  method <- 'ordinal'
  x <- c(1, 1, 2, 2, 3, 2, 2, 3, 2, 3, 3, 2, 1, 2, 2, 1, 1, 1, 2, 2)
  y <- c(1, 1, 2, 1, 3, 3, 3, 2, 2, 3, 3, 3, 2, 2, 2, 1, 2, 1, 3, 3)
  nbLevel <- NULL
  if(method == 'ordinal') {
    nbLevel <- as.factor(c(x,y))
    nbLevel <- length(levels(nbLevel))
  }
  expect_identical(nbLevel, as.integer(3))

  method <- 'binary'
  x <- c(1, 1, 2, 2, 3, 2, 2, 3, 2, 3, 3, 2, 1, 2, 2, 1, 1, 1, 2, 2)
  y <- c(1, 1, 2, 1, 3, 3, 3, 2, 2, 3, 3, 3, 2, 2, 2, 1, 2, 1, 3, 3)
  nbLevel <- NULL
  if(method == 'ordinal') {
    nbLevel <- as.factor(c(x,y))
    nbLevel <- length(levels(nbLevel))
  }
  expect_identical(nbLevel, NULL)
})

test_that('correlation continious', {
  x <- c(1, 2, 3)
  y <- c(3, 4, 5)
  res <- cor(x, y)
  expect_equal(res, 1)

  x <- c(1, 2, 3)
  y <- c(5, 4, 3)
  res <- cor(x, y)
  expect_equal(res, -1)

  x <- c(1, 2, 3)
  y <- c(3, 5, NA)
  res <- cor(x, y, use = 'pairwise.complete.obs')
  expect_equal(res, 1)
})

test_that('function', {
  x <- c(rep(0,19), rep(1,30))
  y <- c(rep(0,12), rep(1,39))
  res <- correlationByType(x,y,method = 'binary')
  expect_equal(res, 0.27, tolerance=0.05)

  x <- c(1, 1, 2, 2, 3, 2, 2, 3, 2, 3, 3, 2, 1, 2, 2, 1, 1, 1, 2, 2)
  y <- c(1, 1, 2, 1, 3, 3, 3, 2, 2, 3, 3, 3, 2, 2, 2, 1, 2, 1, 3, 3)
  res <- correlationByType(x,y,method = 'ordinal')
  expect_equal(res, 0.7828328, tolerance=0.05)

  x <- c(rep('blue', 6), rep('green', 8), rep('brown', 12))
  y <- c(rep('blue', 9), rep('green', 5), rep('brown', 10))
  res <- correlationByType(x,y,method = 'nominal')
  expect_equal(unname(res), 0.1671, tolerance=0.05)
})
