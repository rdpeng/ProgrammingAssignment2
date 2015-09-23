@@ -3,13 +3,40 @@

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
##makeCacheMatrix: This function creates a 
##special "matrix" object that can cache its inverse.

}

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
      
}

## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
      ## Return a matrix that is the inverse of 'x'b
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
} 
