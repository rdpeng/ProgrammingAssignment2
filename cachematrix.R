## The functions can calculate inversed matrix if it is not done before. 
## If it has been calculated, the functions will provide cached results. 


## It creates a matrix object to cache the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){ #set the matrix
    x<<-y
    inverse<<-NULL
  }
  get <- function() x  #get the matrix value
  set_inverse <- function(solve) inverse <<- solve  #set the inverse of matrix
  get_inverse <- function() inverse  #get the inversed one
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Check whether the inverse matrix has value. If yes, show the cached result. If not,
## compute it and cache it in the object created before, return the result.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()  #get the inversed matrix
  if(!is.null(inverse)) {  #check whether it is a null
    message("getting cached data")
    return(inverse)  #return the result
  }
  data <- x$get()
  solve <- solve(data, ...)  #compute the inverse 
  x$set_inverse(solve)  #cache the result
  inverse    ## Return a matrix that is the inverse of 'x'
}
