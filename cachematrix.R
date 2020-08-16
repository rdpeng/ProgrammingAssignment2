## function to create a special type vector, a list, which contains
## functions to set the value of the vector
## function to get the value of the vector
## function to set the value of the inverse of the matrix
## function to get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())    ##First Function
        {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setmatinv <- function(inverse) m <<- inverse
                getmatinv <- function() m
                list(set = set, 
                     get = get, 
                     setmatinv = setmatinv,
                     getmatinv = getmatinv)
                
        }


##Second Function
cacheSolve <- function(x, ...) 
        {
        
        m <- x$getmatinv()
        if(!is.null(m))
                { message("This is already computed! Getting data from cache. 
                          Please wait")
                        return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatinv(m)
        m
        }
## For Coursera Course. Submitted by Faisal. :)