## The functions create and cache the inverse of a given matrix. Here the function solve() is used
## for getting the inverse of the matrix
## It should be noted that it is assumed that the matrix supplied is always invertible


## The first function creates a list containing 4 functions. The 1st function sets the value of the matrix, the 2nd function
## gets the value of the matrix, the 3rd set the value of the inverse and the 4th function gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function()x
  setinverse <- function(solve) i<<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function calculates the inverse of the matrix created above.
## The if statements checks if the inverse has already been computed and stored in the cache.
## If it isn't, it calculates the inverse matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

a<-makeCacheMatrix()
a$set(matrix(1:4,2,2))
cacheSolve(a)