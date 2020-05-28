#Here i have used two functions makeCacheMatrix and cacheSolve to calculate the inverse of a matrix and store it's cached value


#the makeCacheMatrix function creates a matrix from the user input and 
#calculates its inverse and stores its cached value 
#all the values are then stored in a list so that it can be accessed through the '$' operator
makeCacheMatrix <- function(x = matrix()) {
  i <-NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


#the cacheSolve function takes the the input of a created matrix and first checks if any cached values is stored
#and if the value is found then returns it
#if a cached value of inverse of matrix is not found then it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
