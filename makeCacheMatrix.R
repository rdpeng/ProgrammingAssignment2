#we have two functions: "makeCacheMatrix" and "cacheSolve"
#the firs one is "makeCacheMatrix" and consist in set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {    
    x <<- y
    m <<- NULL
  }
  get <- function() {x}   #function to get matrix "x"
  setinverse <- function(inverse) {m <<- inverse}
  getinverse <- function() {m}     #function to get inverse matrix "x"
  list(set = set, get = get,     
       setinverse = setinverse,
       getinverse = getinverse)
}

#here we have the cacheSolve function
cacheSolve <- function(x, ...) {   
  m <- x$getinverse()
  if(!is.null(m)) {         #checking whether "m" is NULL
    message("getting cached data")
    return(m)               #return inverse value
  }
  data <- x$get()
  m <- solve(data, ...)      #calculates the inverse value
  x$setinverse(m)
  m                         
}



