
# Test file for the cachematrix.R file.  Note that this is not part of the assignment,
# and should be disregarded.

source("cachematrix.R")

# value used to set the tolerance for equality between real numbers 
EPSILON <- 1e-8

# is.identity is a helper function that returns true if all numbers on the diagonal are within EPSILON
# of 1, and all other numbers are within EPSILON of 0
is.identity <- function(m) {
  
  rows <- nrow(m)
  cols <- ncol(m)
  
  if (rows != cols) {
    message("Not a square matrix")
    return(FALSE)
  }
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      target <- 0.0
      if (i == j) {
        target <- 1.0
      }
      if (abs(m[i,j] - target) > EPSILON) {
        message(sprintf("Error at %d,%d: %f", i, j, m[i,j]))
        return(FALSE)
      }
    }
  }
  
  TRUE
}

test_that("makeCacheMatrix working", {
  
  test_m <- makeCacheMatrix()
  expect_that( is.null(test_m), is_false())
  
  # test get
  m <- test_m$get()
  expect_that( nrow(m), equals(1) )
  expect_that( ncol(m), equals(1) )
  expect_that( is.na(m[1,1]), is_true() )
  
  # test set and getInverse
  bigM <- matrix(rnorm(1000000), nrow=1000, ncol=1000)
  test_m$set(bigM)
  expect_that( identical(test_m$get(), bigM), is_true() )
  expect_that( is.null(test_m$getInverse()), is_true() )
  
  # test setInverse and getInverse
  bigI <- matrix(rnorm(1000000), nrow=1000, ncol=1000) # note that this is not inverse. Placeholder for tests.
  test_m$setInverse(bigI)
  expect_that( identical(test_m$getInverse(), bigI), is_true() )
  
})

test_that("cacheSolve working", {
  
  bigM <- matrix(rnorm(1000000), nrow=1000, ncol=1000)
  test_m <- makeCacheMatrix(bigM)
  expect_that( identical(test_m$get(), bigM), is_true())
  
  # test solve
  bigI <- cacheSolve(test_m)
  expect_that( nrow(bigI), equals(nrow(bigM)) )
  expect_that( ncol(bigI), equals(ncol(bigM)) )
  expect_that( is.identity(bigM %*% bigI), is_true())
  
  # reset inverse
  
  test_m$set(bigM)
  expect_that( is.null(test_m$getInverse()), is_true() )
  slow <- system.time(cacheSolve(test_m))
  fast <- system.time(cacheSolve(test_m))
  # this is crude, but all subsequent calls should be quicker than the first.  Will repeat test a few times to be sure.

  fast <- system.time(cacheSolve(test_m))
  expect_that( fast[1] < slow[1], is_true() )
  fast <- system.time(cacheSolve(test_m))
  expect_that( fast[1] < slow[1], is_true() )
  fast <- system.time(cacheSolve(test_m))
  expect_that( fast[1] < slow[1], is_true() )
  fast <- system.time(cacheSolve(test_m))
  expect_that( fast[1] < slow[1], is_true() )
  
  # make sure they return the same value
  expect_that( identical(cacheSolve(test_m), bigI), is_true())
  
})


