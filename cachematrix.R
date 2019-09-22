# # # # ## My functions try to compute the inverse of a matrix and cache it
# # My first function, makeCacheMatrix, creates a special matrix and can also 
# cache its inverse. 
# # # This function returns a list which is essentially a set of functions, 
# which can:
# # # 1. Set the matrix object 2. Get the matrix object.
# # # 3. Set the inverse matrix object 4. Get the inverse of the matix 

        makeCacheMatrix <- function(x = matrix()) {
                xi <- NULL
                set <- function(y) {
                        x1 <<- y
                        xi <<- NULL
                }
                get <- function() x
                setinv <- function(inv1) xi <<- inv1
                getinv <- function() xi
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
                
        }
        
# cacheSolve is a function that checks if a previously 
# computed inverse of the "special" matrix is available, if so returns it.
# If not computes the inverse, sets that computed as the inverse and
# returns it.
        
        cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                xi <- x$getinv()
                if(!is.null(xi)) {
                        message("getting cached data")
                        return(xi)
                }
                x$set(matrix1)
                data <- x$get()
                xi <- solve(data, ...)
                x$setinv(xi)
                xi
        }
        
        
