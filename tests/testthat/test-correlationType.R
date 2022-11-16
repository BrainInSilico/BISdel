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

test_that('contingency table', {
  x <- c(0, 1, 0, 1, 0, 1, 0, 0)
  y <- c(0, 1, 0, 1, 0, 1, 1, 1)
  contingency <- as.factor(ifelse(,"Above","Below"))
  suppressWarnings(res <- tetrachoric(x, y)$rho)
  expect_equal(res, 1)

  x <- c(0, 1, 0, 1, 0, 1, 0, 0)
  y <- c(1, 1, 1, 1, 1, 1, 1, 1)
  suppressWarnings(res <- tetrachoric(x, y)$rho)
  expect_equal(res, -1)
})
