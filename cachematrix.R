##These two functions cache and compute the inverse of a square matrix using solve()
## The matrix supplied must always be invertible
## To see if this works: 
## test <- matrix(c(6,3,9,4), 2,2)
## M < makeCacheMatrix(test)
##cacheSolve(M)
#The result should be:          [,1] [,2]
##                    [1,] -1.333333    3
##                    [2,]  1.000000   -2

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
       ## Initialize n as NULL which holds the cached value (this is NULL if nothing is cached)
       n <- NULL
      ## Set the value of the matrix
      set <- function(y) {
          x <<- y
          n <<- NULL
       }
       ## Returns the value of the matrix x
      get <- function() x
      
       ## Set the value of the inverse
       setinverse <- function(inverse) n <<- inverse
      
       ## Returns the cached value  of the inverse of the matrix x
      getinverse <- function() n
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ##
         n <- x$getinverse()
         ## Check that the cached inverse is not null
           if(!is.null(n)){
                  message("getting cached data")
                 ## Return the cached inverse
                    return(n)
              }
           ##load values of matrix x as variable mat
            mat <- x$get()
            ## Caluclate the inverse of the matrix using solve and store it
               n <- solve(mat, ...)
               x$setinverse(n)
               
           #Return the inverse
           n
         }