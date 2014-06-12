## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates aspecial matrix that can cache its inverse by creating a special matrix and defining the moments to implement

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  #set value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x #get value of the matrix
        setinv <- function(solve) m <<- solve   #set the value of the matrix inverse
        getinv <- function() m  #get value of the matrix inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

# This function computers the inverse of the special matrix .

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                
        }
        data <- x$get()
        m<-solve(data,...)
        x$setinv(m)
        m
        
}

