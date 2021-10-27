INSTRUCTION:

<<-  In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores and calculate matrix of the given functions and cache's its mean.

The first function, makeVector creates a special “vector”, which is really a list containing a function to:

1. set the value of the vector
2. get the value of the vector
3. set the value of the mean
4. get the value of the mean


       
       makeVector <- function(x = numeric())  {
        
        m <- NULL
       
       set <- function(y) {
                x <<- y
                m <<- NULL
        }
       
       get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)       }


<<- The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

        
        cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
                data <- x$get()
           m <- mean(data, ...)
           x$setmean(m)
           m
        }


Assignment: Caching the Inverse of a Matrix
less 
Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.


Write the following functions:

<<- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

1. set the value of the vector
2. get the value of the vector
3. set the value of the mean
4. get the value of the mean

Answer:
     
     
     makeCacheMatrix <- function(x = matrix()) {
     
     inv <- NULL
     set <- function(y){
      x <<- y
     inv <<- NULL }
     get <- function() x
      setInversematrix <- function(solveMatrix)
      inv <<- solveMatrix
     getInversematrix <- function() inv
      list(set = set, get = get, 
      setInversematrix = setInversematrix, getInversematrix = getInversematrix)  }


<<- cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

Answer: 
    
    cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
      inv <- x$getInversematrix()
      if(!is.null(inv)){
                 message("getting cached data")
                 return(inv)}
    data <- x$getInverse()
    inv <- solve(data)
     x$setInversematrix(inv)
    inv      }

* Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
* For this assignment, assume that the matrix supplied is always invertible.

