## Yisheng Wang 2019/02/10
## Put comments here that give an overall description of what your
## 1 makeCacheMatrix: This function creates a special "matrix" object that can caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # 'makeCacheMatrix' 
  # makes a cache matrix from a given matrix
  #---------------------------------------------------------
  # 1. initialize the cache Matrix 'cacheMatrix'
  # assign the value NULL for the first initialization
  
  cacheMatrix <- NULL
  
  # 2. define the method named 'setMatrix'
  
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  # 3. define the method named 'getMatrix'
  # return the matrix 'x'
  
  getMatrix <- function() x
  
  # 4. define the method named 'setCache'
  
  setCache <- function(inverse) cacheMatrix <<- inverse
  
  # 5. define the method named 'getCache'
  # that will return the cached inverse of 'x'
  
  getCache <- function() cacheMatrix
  
  # 6. list the names of all methods that will be known to the outside world
  # although the name in this list can be different from the name of the methods defined above
  # I choose the same name as the methods defined above for simplicity and coherence
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
  
}


## 2 cacheSolve: This function computes the inverse of the special"matrix" returned by makeCacheMatrix above. If the inverse has already 
## has been calculated(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  
  # 'cacheSolve'
  # return the inverse of a given matrix utilizing the cache
  
  # 1. check the content of cache matrix
  
  cacheMatrix <- x$getCache()
  
  # 2. if the content is not null then: return the result 
  
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  
  # 3. if the content is empty then: 
  # get the matrix, create, set, update and return the cache matrix
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
  
}
