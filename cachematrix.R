##Inversion of matrix

## to create special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y)
{
      x <<- y
      inv <<- NULL
}
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## to compute inverse of special matrix returned by the function above

cacheSolve <- function(x, ...)
{
## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) 
{
    message("getting cached data")
    return(m)
}
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
