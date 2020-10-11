## Overall this functions takes a single argument that must be a matrix or
## something that can be coerced into a matrix. It returns a list of pointers to
## functions inside makeCacheMatrix. It makes the inverse of the matrix and
## stores it in the object m.

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) { ##set clears out m 
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list (set=set, get=get, setinv=setinv,getinv=getinv)
}


##This function takes the list made from makeCacheMatrix as its input. It calls
## the getinv function and puts it into m, tests to see if m is not null. If m
## is not null it will return m. In this case m will be the inverse of the
## matrix that was made in makeCacheMatrix. If m is NULL, the function gets the
## the original matrix from makeCacheMatrix, 
cacheSolve <- function(x,...){
  m <- x$getinv()
  if(!is.null(m)){ 
    print("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...) 
  x$setinv(m) 
  m 
}