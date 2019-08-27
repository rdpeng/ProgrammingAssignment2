## Put comments here that give an overall description of what your
## functions do
##Function makeCacheMatrix: upon input a matrix, set the matrix's value, 
#get the value of the matrix, set the inverse matrix and get the inverse matrix. the matrix object can be cached.
## Write a short comment describing this function
#the <<- operator is used to assign a value 
#to an object in an environment that is 
#different from the current environment.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {                         #set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                          #get the value of the Matrix    
  setinverse <- function(solve) inv <<- solve  #set the value of the invertible matrix
  getinverse <- function() inv                 #get the value of the invertible matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve receive output and check for cached result
#if it is emptied, the function take the original data and 
#solved it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                        #get value from makeCachematrix
  if(!is.null(inv)) {                          #if the cache not emptied
    message("getting cached data")
    return(inv)                                #return the data
  }
  data <- x$get()                              #get the original data
  inv <- solve(data, ...)                      #use solve 
  x$setinverse(inv)
  inv 
  ## Return a matrix that is the inverse of 'x'
}
