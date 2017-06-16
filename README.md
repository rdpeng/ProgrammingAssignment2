
### Assignment: Caching the Inverse of a Matrix

    makeVacheMatrix <- function(x = matrix()) 
    {
            m <- NULL
            set <- function(inverse) 
            {
                    x <<- inverse
                    m <<- NULL
            }
            get <- function() 
            {
            x
            }
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
    }


    cacheSolve <- function(x) {
            m <- x$getinverse()
            if(!is.null(m)) {
                    print("getting cached inverse matrix")
                    ## return a matrix that is the inverse of 'x' if inverse already exist
                    return(m)
            }
            data <- x$get()
            m <- solve(data)
            x$setinverse(m)
            m
    }


