##  In order to built the makeCacheMean function, we need to follow the next steps: 
## (1) set the value of the matrix by initializing two objetcs called "n" and "w" (empty numeric vector) that will be used later.
##     set() takes  "y" as a argument.  
## (3) get the those values using the get() option within the object created
## (4) set the value of the inverse matrix
## (5) get the value of the inverse matrix.
## (6) assign each of these functions an element within a list. 

makeCacheMatrix <- function(x = matrix()) {
              n <- NULL
            set <- function(y) {
             x <<- y
             w <<- NULL
  }
  get <- function() x
  setsolution <- function (inverse) n <<- inverse
  getsolution <- function () n 
  list(set = set, get = get, setsolution = setsolution, getsolution = getsolution)
}


## The cacheSolve function computes and retrieves the inverse of the matrix defined before.
## This function is needed as it will populate or retrieve the inverse of the matrix defined on part one. 
## Like we did on makeCacheMatrix, we need to start a single argument "x".
## Then, using getsolution(), the function will get the value of the matrix if it is different from null. 
## Finally, we need to set the inverse of the matrix.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getsolution()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  n <- solve(data, ...)
  x$setsolution(n)
  n
}

##Althought it is not a requirement of the assigment, I provide an example of how this functions works.
##First, we need to define a matrix. In this case, I use a 3x3 matrix.
x <- matrix(c(1,0,5,2,1,6,3,4,0),nrow=3, ncol=3)
##       [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## Then, I execute the two functions created above and see the inverse of the matrix.
a <- makeCacheMatrix(x)
cacheSolve(a)
getting cached data
##       [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## As we can see, after running cacheSolve function, I get the inverse or matrix called x

