## This functions take a matrix as argument and search in the cache to 
## check if its inverse is recorded there, if so, they return it and print it
## and if not, they calculate and save it, in order to use it in future queries 

## This function receives a matrix and returns a set of functions to manipulate
## such matrix. It allows to create private environments

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     set <- function (n) {
          x <<- n
          inv <<- NULL
     }
     
     get <- function () x

     setInverse <- function (inverse) inv <<- inverse

     getInverse <- function () inv

     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
     
}


## This function receives a matrix as argument and checks if its inverse
## is saved in the cache, if so, it returns it and otherwise, it calculates it
## and saves it into the cache, to avoid future calculations of that same matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if (!is.null(m)) {
          message("Tomando el valor de la memoria cache")
          return (m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m     
}
