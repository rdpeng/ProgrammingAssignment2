## Begin R Code. Program Assignment 2
## the following function create a standard matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matinv <<- inverse
  getinverse <- function() matinv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function below first check if matrix is a square matrix, 
## if not appropriate message is returned
## Then it checks if det(x) <> 0 i.e. matrix is invertible; 
## if not appropriate message is returned
## Then it checks if the inverse is already calculated for the matrix, 
## if so, inverse is returned from cache
## If inverse is not calculated then inverse is calculated, cached and returned 

cacheSolve <- function(x, ...) {
  
  # Check if matrix is a square matrix
  data <- x$get()
  if (nrow(data) != ncol(data)) {
    message("Input matrix is not a square matrix. Exiting.")
    return(0)
  }
  ## Check if matrix is invertible i.e. det(M) <> 0 
  chk_num <- try(det(data),silent=T)
  if (!is.numeric(chk_num) || chk_num == 0) {
    message("Input matrix is not invertible. Exiting.")
    return(0)
  }
  ## Check if inverse available in cache
  matinv <- x$getinverse()
  if(!is.null(matinv)) {
    message("getting cached data.")
    return(matinv)
  }
  ## Compute inverse, cache and return inverse
  data <- x$get()
  matinv <- solve(data)
  x$setinverse(matinv)
  matinv
}
## End of R code ##

## Test1:
## x <-cbind(1, 1:3, c(2,0,1))
## mt <- makeCacheMatrix(x)
## mt$get()
## [,1] [,2] [,3]
## [1,]    1    1    2
## [2,]    1    2    0
## [3,]    1    3    1
## 
## first time, inverse is calculated
## cacheSolve(mt)
## [,1]       [,2]       [,3]
## [1,]  0.6666667  1.6666667 -1.3333333
## [2,] -0.3333333 -0.3333333  0.6666667
## [3,]  0.3333333 -0.6666667  0.3333333## 
## 
## Test2: check that data is retrieve from cache
## cacheSolve(mt)
## getting cached data.
## 
## [,1]       [,2]       [,3]
## [1,]  0.6666667  1.6666667 -1.3333333
## [2,] -0.3333333 -0.3333333  0.6666667
## [3,]  0.3333333 -0.6666667  0.3333333
##
## Test3: not a square matrix
## x <-cbind(1:3, 5:7)
## mt <- makeCacheMatrix(x)
## mt$get()
##      [,1] [,2]
## [1,]    1    5
## [2,]    2    6
## [3,]    3    7
## cacheSolve(mt)
## Input matrix is not a square matrix. Exiting.
## [1] 0
## 
## Test4: Matrix is not invertible
## x <-cbind(1:3, 4:6, 7:9)
## mt <- makeCacheMatrix(x)
## mt$get()
## [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9
## cacheSolve(mt)
## Input matrix is not invertible. Exiting.
## [1] 0
## Test4: Matrix with det(M) = 0
## x <-cbind(1:3, 4:6, 1:1)
## mt <- makeCacheMatrix(x)
## mt$get()
## [,1] [,2] [,3]
## [1,]    1    4    1
## [2,]    2    5    1
## [3,]    3    6    1
## cacheSolve(mt)
## Input matrix is not invertible. Exiting.
## [1] 0
## End of Test
