## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setMatrix <- function (solve) m <<- solve
        getMatrix <- function () m
        list(set = set, 
             get = get, 
             setMatrix = setMatrix, 
             getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getMatrix()
        if (!is.null(m)) {
                message("Get inverse Matrix from cache.")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}

testMatrix <- function (y){
        
        temp = makeCacheMatrix(y)
        cacheSolve(temp)
}

