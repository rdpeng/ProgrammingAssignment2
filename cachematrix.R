## Put comments here that give an overall description of what your
## functions do

## Creates a list of functions to set and get cached values of matrices and their inverses

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## Returns a matrix that is the inverse of 'x'
## using a cached value if it exists
## otherwise calculates it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}

## Sample Usage

## 1. Create a matrix: 
## x = matrix(c(c(1,2,3),c(3,4,5),c(7,8,8)),3,3)

## 2. Create a cachematrix:
## cm = makeCacheMatrix(x)

## 3. Verify cm value:
## > cm$get()
## [,1] [,2] [,3]
## [1,]    1    3    7
## [2,]    2    4    8
## [3,]    3    5    8

## Get cm inverse - not cached:
## > cacheSolve(cm)
## [,1] [,2] [,3]
## [1,]   -4  5.5   -2
## [2,]    4 -6.5    3
## [3,]   -1  2.0   -1

## Get cm inverse - cached:
## > cacheSolve(cm)
## getting cached data
## [,1] [,2] [,3]
## [1,]   -4  5.5   -2
## [2,]    4 -6.5    3
## [3,]   -1  2.0   -1

