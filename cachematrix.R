## Introduction

## Hello! So what is included in this .R document? There are two functions,
## one named makeCacheMatrix(), and one named cacheSolve().
## Generally, the makeCacheMatrix() will require you to make a matrix
## and creates a list of four functions that can be referenced by other
## functions. The cacheSolve() function will reference the function list
## and the matrix created in makeCacheMatrix, see if the answer
## exists already in its cache, and solve for the inverse of the matrix.
## Some test data exists at the bottom of the file that you can use to test
## the function. There are also expected results for reproducability listed
## below as well.


## makeCacheMatrix() Description
## This function requires you to enter a matrix. It saves the matrix for later
## use, and also includes four functions in a list that can be called later.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() Description
## This function retrieves the list created in the makeCacheMatrix() function above,
##and executes the functions created therein. It does check if the matrix inverse has
## already been created, and if it has, it "gets the cached data". Regardless, it provides
## the inverse to the matrix created in the function above using the solve() function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## Test Variables
## c=rbind(c(1, -1/4), c(-1/4, 1)) ## create a test matrix called c
## d=rbind(c(4,7),c(2,6)) ## create a test matrix called d
## testc <- solve(c) ## calculate the inverse of c ahead of time to use with "identical()" function in the command line
## testd <- solve(d) ##calculate the inverse of c ahead of time to use with "identical()" function in the command line

## Testing Comments
## In order to test whether or not the Cacheing and Solving functions were working I wanted to create test data to compare it to,
## and across multiple matrices to see if there is actual cacheing going on. If there was, the "getting cached data" message should
## appear within the console at some point.

## Testing Console Code (to copy and paste to see if it works)
## > cachec <- makeCacheMatrid(c)
## > cached <- makeCacheMatrix(d)
## > retrievec <- cacheSolve(cachec)
## > retrieved <- cacheSolve(cached)
## getting cached data ## this is what I wanted to make sure I saw at some point in the code.
## identical(testc, retrievec) ## I calculated and saved the inverse of the matrix c to test with using identical()
##[1] TRUE
## identical(testd, retrieved) ## Same as listed above. Just wanted to make sure everything was copacetic.
##[1] TRUE