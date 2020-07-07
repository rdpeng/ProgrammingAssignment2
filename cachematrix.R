makeCacheMatrix <- function( m = matrix() ) 
  {
  
  inv <- NULL
  
  set <- function( matrix ) 
  {
    m <<- matrix
    inv <<- NULL
  }
  
  get <- function() 
  {
    m
  }
  
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  getinv <- function() {
    inv
  }
  
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}

cacheSolve <- function(x, ...) 
{
  
  m <- x$getinv()
  
  if( !is.null(m) ) 
    {
    message("getting cache")
    return(m)
    }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinv(m)
  m
}
