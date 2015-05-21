#set the value of the matrix
#get the value of the matrix
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cacheSolve <- function(x, ...) {
  m <- x$getmean() #get mean
  
  data <- x$get()
  invx <- solve(data) # matrix inversion, x inversion
  m <- mean(invx, ...)
  x$setmean(m)
  # check first or cached value
  if(!is.null(m)) {
    message("this is cached calculation")
    return (m)    
  } 
    
    m 
}


#testing functions, first create a matrix

l <- matrix(runif(1000), 5,5)
#print l
l
# test makeCacheMatrix
makeCacheMatrix(l)
# test cacheSolve

o <-makeCacheMatrix(l)
o
p <- cacheSolve(o)
p
