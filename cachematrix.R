## this functions cache potentially time-consuming computations.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
inverse_mat <- NULL
set <- function(y){
        x <<- y

      inverse_mat <<- NULL
        
}
get <- function() x
setsolve <- function(solve) inverse_mat <<- solve
getsolve <- function() inverse_mat

list(set = set, get = get, setsolve = setsolve, 
     getsolve = getsolve)

        inverse_mat


}


##  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse_mat <- x$getsolve()

        if(!is.null(inverse_mat)){
                message("Getting data from cache")
                return(inverse_mat)
        }


        else if(nrow(x) != ncol(x)){
                data <- x$get()
                inverse_mat <- solve(x)
                x$setsolve(inverse_mat)
                inverse_mat       
        }
                message("x is not a invertible matrix !")


        data <- x$get()
        inverse_mat <- solve(x)
        x$setsolve(inverse_mat)
        inverse_mat


        }
