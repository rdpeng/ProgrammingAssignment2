
## makeCacheMatrix - creates function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
             x <<- y
             m <<- NULL
         }
       get <- function() x
       setinverse <- function(inverse) m <<- inverse
       getinverse <- function() m
       list(set=set, get=get,
            setinverse=setinverse,
            getinverse=getinverse)
   }


## The following function calculates the inverse of the matrix created with the above function.
## First checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix m and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()  ## Return a matrix that is the inverse of 'x'
       if(!is.null(m)) {
             message("getting cached data.")
             return(m)
         }
       data <- x$get()
       m <- solve(data)
       x$setinverse(m)
       m
   }

