## This function creates a special matrix object which is really a list containing a function to
##    1. Set value of matrix
##    2. Get value of matrix
##    3. Set value of inverse
##    4. Get value of inverse

makeCacheMatrix <- function(x = matrix()){
      m <- NULL                 ## Define cache of 'm'
      set <-function(y){        ## Assign input matrix of 'y' to variable 'x' in parent environment
      x <<- y
      m <<- NULL                  
}
      get <- function()x        ## Return matrix 'x'
      setinverse <- function(inverse)m <<- inverse ## Sets the cache 'm' = inverse of x in parent environment
      getinverse <- function()m                    ## Returns the cached inverse of 'x'
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
##The following calculates the inverse of the special matrix created in the above function.  It checks if the inverse
##has been calculated  and if so get the inverse from the cache.  Otherwise it calculates the matrix inverse and sets
##the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse
      if(!is.null(m)){
            message("getting cached data")
            return(m)
}
      data <- x$get()
      m <- solve(data,...)
      x$setinverse(m)
      m
}
