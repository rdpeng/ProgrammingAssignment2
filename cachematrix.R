## The function makeCacheMatrix creates a matrix object and cache its values.
## The function cacheSolve checks if the computation for inverse has been done and performs
## the calculation or if it has been done, take the value from cache


## makeCacheMatrix creates a list containing the funtion to 
#1 set the value of the matrix, 
#2 get the value of the matrix
#3 set the value of the inverse of the matrix
#4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve first check if the inverse of the matrix has been calculated. 
#If not yet calculated, it calculates the inverse matrix
#If already calculated, it gets the inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

### description of learning steps - I mostly relied on hints 
##practice1 start with the example
makeVector <- function(x = numeric()) {
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

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

##practice2 Read this to understand the makeVector function: https://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r
##practice3 Watch lecture/instruction from youtube https://www.youtube.com/watch?v=9WXQa9NlGDs

##practice4 Practice and test functions

xmatr<-matrix(c(3:6), ncol=2, nrow=2) ##practice5 create a matrix for test purpose 

## test created functions
cxmatr<-makeCacheMatrix(xmatr)  #practice6 creates a special “matrix” object that can cache its inverse 
cxmatr$get()                    #practice7 get the value of xmatr 
cxmatr$getInverse()             #practice8 get the inv (NULL) 
cacheSolve(cxmatr)              #practice9 compute the inverse of xmatr 
cacheSolve(cxmatr)              #practice10 check the inverse of xmatr and get the values in Cache
cxmatr$getInverse()             #practice11 get the calculated inverse of xmatr
