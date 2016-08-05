## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}


Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
(1)get is a function that returns the vector x stored in the main function.
(2)set is a function that changes the vector stored in the main function.
(3)setmean and getmean are functions very similar to set and get.
(4)They don’t calculate the mean, they simply store the value of the input in a variable m.
(5)into the main function makeVector (setmean) and return it (getmean).Like:
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

Function “cacheSolve” computes the inverse of the special “matrix” (which is the input of cachemean) returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix. Such as:
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
