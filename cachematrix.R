Skip to content
Search or jump to…

Pull requests
Issues
Marketplace
Explore
 
@pabhisek 
Learn Git and GitHub without any code!
Using the Hello World guide, you’ll start a branch, write comments, and open a pull request.


prooftv
/
ProgrammingAssignment2
forked from rdpeng/ProgrammingAssignment2
0
0134k
Code
Pull requests
Actions
Projects
Wiki
Security
Insights
ProgrammingAssignment2/cachematrix.R
@prooftv
prooftv Update cachematrix.R
Latest commit 229d158 4 days ago
 History
 2 contributors
@rdpeng@prooftv
58 lines (47 sloc)  1.46 KB
  
## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix
# The first function, makeCacheMatrix, creates a special "matrix", which is really a 
# list containing a function to:
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse of the matrix using the solve() function
# 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #calculates the inverse of the matrix and caches the result
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## THE FOLLOWING ARE A LIST OF COMMANDS TO TEST THESE FUNCTIONS

#>setwd("")
#>source("cacheMatrix.R")
#>a <- makeCacheMatrix(matrix(rnorm(25),nrow = 5,ncol = 5))
#>a$get()
#>a$getinverse()
#>cacheSolve(a)
#>a$getinverse()  # this is only to show you that the mean has been stored and does not affect anything
#>cacheSolve(a)
© 2020 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
Pricing
API
Training
Blog
About
