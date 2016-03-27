## makeVector Function
## from Coursera
makeVector <- function(x = numeric()) {
			m<- NULL
			set <- function(y) {
				x <<- y
				m <<- NULL
				}
			get <- function() x
			setmean <- function(mean) m <<- mean
			getmean <- function() m
			list(set=set, get=get,
				setmean = setmean,
				getmean = getmean)
		}

cachemean <- function(x, ...){
	m<- x$getmean()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
		}
		data <- x$get()
		m <- mean(data,...)
		x$setmean(m)
		m
	}

## creates a matrix that can cache its inverse

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
## Computes the inverse of the special matrix returned by the function above

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