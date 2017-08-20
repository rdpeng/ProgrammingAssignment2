# makeCacheMatrix creates a matrix and superassigns the matrix and inverse values 
# so that the cachesolve function can retrieve them and calculate the matrix inverse

# function test 1: 
# > cachesolve(makeCacheMatrix(1:4, 2, 2))

# function test 2: 
# > z <- makeCacheMatrix(1:4, 2, 2)
# > cachesolve(z)


# makeCacheMatrix creates a matrix, storing it (internally) in the 'x' variable
# it also defines functions for retriving and manipulating the matrix (x)  and its inverse (i)

makeCacheMatrix <- function(x = matrix(), n, j) {
  dim(x) <- c(n, j)
  #the matrix must be square in order to caluculate the inverse 
  #make sure that n == j, otherwise return a warning; 
  if(n != j) {
    warning("the matrix must have the same number of rows and columns")
  }
  i <- NULL 
  set <- function(y) { 
    x <<- y            # '<<-' assigns values to the variables in the enclosing environment, 
    i <<- NULL         # 
  }                    
  get <- function() x  # the matrix data and the 'i' value is retrieved by cachesolve when 'get' is invoked
                       # the inverse is calculated and then set with 'setinverse'
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# cachesolve calculates the inverse of the matrix created by makeCacheMatrix
# it makes this calculation only if the inverse has not already been calculated and stored by makeCacheMatrix

cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
