# The MakeCacheMatrix function is made up
# of 4nverse of the user-supplied matrix as
# an ar functions: the first one known as set
# is in charge of receiving the argument which
# is an array to which the inverse will be
# calculated. The second function called get
# shows the array stored in the set function.
# The third function called setsolve receives
# the igument. Finally, the getsolve function is
# responsible for displaying or printing said
# inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <-  NULL
  set <- function(x1){
    x <<- x1
    inv <<- NULL
  }
  get <- function()x
  setsolve <- function(solve) inv<<- solve
  getsolve <- function () inv
  list (set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve) 
}


# # This function check if the inverse of the matrix
# is found in the cache, if it is found, the program
# prints the inverse and otherwise calculates the inverse 
# # and saves it.

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}