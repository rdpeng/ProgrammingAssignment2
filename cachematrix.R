## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}> source("ProgrammingAssignment2/cachematrix.R")
> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(my_matrix)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$getInverse()
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
> my_matrix$get()
[,1] [,2]
[1,]    2    1
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> cacheSolve(my_matrix)
getting cached data
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> my_matrix$getInverse()
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333

Another Example:open.account <- function(total) {
  list(
    deposit = function(amount) {
      if(amount <= 0)
        stop("Deposits must be positive!\n")
      total <<- total + amount
      cat(amount, "deposited.  Your balance is", total, "\n\n")
    },
    withdraw = function(amount) {
      if(amount > total)
        stop("You don't have that much money!\n")
      total <<- total - amount
      cat(amount, "withdrawn.  Your balance is", total, "\n\n")
    },
    balance = function() {
      cat("Your balance is", total, "\n\n")
    }
  )
}n
####Test 3 [3*3 Matrix] --> singulat Matrix #####
-#matrix(1:9
  ,3,3) is not possible (singule matrix) because it is giving det(A)  = 0
  -#matrix = 1/det(A)[3,-6,3,-6,12,-6,3,-6,3] |det(A) = 1/(1*3 +4* (-6) + 7 *3) = 1/0
  -TestMatrix <- matrix(1:9,3,3)
-TestMatrix
-
  -CacheMatrix <- makeCacheMatrix(TestMatrix)
-CacheMatrix$getMatrix()
-CacheMatrix$getInverse()
-
  -cacheSolve(CacheMatrix)
-cacheSolve(CacheMatrix)
-
  -####Test 4 [3*3 Matrix]#####
-TestMatrix <- matrix(1:8,3,3)
-TestMatrix
-
  -CacheMatrix <- makeCacheMatrix(TestMatrix)
-CacheMatrix$getMatrix()
-CacheMatrix$getInverse()
-
  -cacheSolve(CacheMatrix)
-cacheSolve(CacheMatrix)
-
  -
  -####Test 5 [4*4 Matrix]#####
-TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
-TestMatrix
-
  -CacheMatrix <- makeCacheMatrix(TestMatrix)
-CacheMatrix$getMatrix()
-CacheMatrix$getInverse()
-
  -cacheSolve(CacheMatrix)
-cacheSolve(CacheMatrix)
-
  -
  -####Test 6 [4*4 Matrix]  --> singular Matrix #####
-TestMatrix <- matrix(5:21,4,4)
-TestMatrix
-
  -CacheMatrix <- makeCacheMatrix(TestMatrix)
-CacheMatrix$getMatrix()
-CacheMatrix$getInverse()
-
  -cacheSolve(CacheMatrix)
-cacheSolve(CacheMatrix)

  


  


