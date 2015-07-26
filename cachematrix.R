## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x=matrix()){
        inver <- NULL
        
        #set the matrix
        set <- function(y){
                matrix <<- y
                inver <<- NULL
        }
        #get the matrix
        get <- function(){
                matrix
        }
        #set the inverse of the matrix
        setinverse <- function(inverse) {
                inver <<- inverse
        }
        
        #get the inverse of the matrix
        getinverse <- function() {
                inver
        }
        # Return the list of methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        #get a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        # return the inverse value
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }

        #get the matrix again
        data <- x$get()
        
        #calculate the inverse
        inver <- solve(data) %*% data
        
        #store the inverse
        x$setinverse(inver)
        #return the inverse
        inver
}
