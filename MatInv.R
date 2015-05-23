makeCacheMatrix <- function(x = numeric()) {  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){x}
  setinv <- function(solve){
      m <<- solve 
  }
  getinv <- function(){m}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
