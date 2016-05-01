## Calculating the inverse of a square invertible matrix, using the cache mechanism to save time if the inverse is already solved.

## Initializes the caching mechanism

makeCacheMatrix <- function(x = matrix()) {
inv=NULL
  set=function(y)
  {
    x<<- y
    inv<<- NULL
    
  }
get= function() {x}
set_Inv= function(inverse){ inv<<- inverse }
get_Inv= function(){ inv }
list( set= set, get= get, set_Inv= set_Inv, get_Inv= get_Inv)

}


## Retrives matrix inverse from cache if already solved, else solves freshly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 inv= x$get_Inv()
    if(!is.null(inv)) {
       message("getting cached data")
      return(inv)
      
    }
  mat.data= x$get()
  inv=solve(mat.data, ...)
  x$set_Inv(inv)
  return(inv)

 }
