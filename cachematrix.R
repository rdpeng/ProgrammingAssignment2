## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function first establishes the objects x and s. x being a matrix input and s being the calculated inverse of that matrix. 
## set get setMat and getMat are all of the getters and setters that allow for the establishment and transformation of the input matrix. 
## using the main function, we set up the cacheSolve function to take our input matrix and set the value of the cached inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setMat <- function(solve) s <<- solve
      getMat <- function() s
      list(set = set, get = get,
           setMat = setMat,
           getMat = getMat)
}


## Write a short comment describing this function
## cacheSolve takes the getters and setters of the previous makeCacheMatrix functions and uses them to cache or display the cache 
  ##which is the inverse of the input matrix
##s looks for the solve function in the getMat section of the previous function and tells us whether or not a cache is currently being stored. 
##if there is something stored in the cache, the function will return that data
##if there is not something stored in the cache, the function will proceed to solve the inverse of the matrix, store it in the cache, and display the 
  ##solution. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s<- x$getMat()
      if(!is.null(s)) {
        message("getting cached data")
        return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setMat(s)
      s
}
