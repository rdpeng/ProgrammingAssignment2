#create/retrieve a cached inverse matrix using the solve() function, two branches can be accessed with subsetting ($get, $set)
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #apply computation / mean is simply replaced by 'solve'
  getinverse <- function() m #pick up result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#retrieve the matrix inverse from makeCacheMatrix(), either calculated via 'set' or from chache via 'get'
cacheSolve <- function(x, ...) 
{
  m <- x$getinverse() #check the cache
  
  if(!is.null(m))  #if m is not empty, load from cache
  {
    message("getting cached data")
    return(m) #return cached result
  }
  
  data <- x$get() 
  m <- solve(data, ...) #'mean' replaced by solve
  x$setinverse(m) #if not cached, apply the function
  m
}