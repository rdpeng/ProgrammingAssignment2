#start of the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  # initialize the stored inverse value so it is NULL
  inverse <- NULL
  
  # to do the setting of the value of the matrix
  set <- function(y) {
    x <<- y
    #set again to NULL the inverse since the matrix has change
    inverse <<- NULL   
  }
  #get function to get the value of the matrix
  get <- function() x
  #to set the inverse of the matrix
  setinverse <- function(inverse_) inverse <<- inverse_
  #to get the inverse of the matrix
  getinverse <- function() inverse
  
  #the returned list of the functinos used above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)    
}



#start of the cacheSolve function
cacheSolve <- function(x, ...) {
  #look for the matrix if it is already cached
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    #return the inverse of the matrix since it's already cached
    return(inverse)
  }
  #since it's nos chached, get the matrix 
  data <- x$get()
  #compute the inverse for the matrix we got
  inverse <- solve(data, ...)
  #set the inverse of the matrix, that is cache-ing it
  x$setinverse(inverse)
  #return the value of the inverse
  inverse
}
