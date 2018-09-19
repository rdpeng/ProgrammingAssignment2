
## This function it returns a list of funtions to operate over the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The funtion below returns the inverse of a matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(nrow(x) == ncol(x) && det(x) != 0){ ## Is the matrix inversible?
          m <- x$getinverse
          if(!is.null(m)){ ##is the inverse already calculated?
                    message("getting cached data")
                    return(m)
          }
          data <- x$get 
          m <- solve(data)
          x$setinverse(m)
          m
        } else message("The matrix has not inverse") 
}

