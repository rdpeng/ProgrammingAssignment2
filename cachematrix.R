## The functions given are assigned to cache the inverse of a matrix
## Produces an object matrix which can cache its inverse 

makeCacheMatrix <- function( m- matrix () ) {

        ## Load the inverse property 
       i <- NULL
        
        ##Procedure to view the matrix
        set <- function (matrix) {
                m <<- matrix
                i <<- NULL 
        }

## Procedure to obtain the matrix 
 get <- function () {
     ## Return the matrix 
      m
}
        
## Procedure for the matrix to become an inverse 
setInverse <- function(inverse) {
           i <<- inverse
}

## Procedure to obtain the inverse of the matrix
getInverse <- function() {
      ## Return the inverse
         i 
}
        
## Return the following procedures 
list (set = set, get = get,
      setInverse = setInverse, 
      getInverse = getInverse) 
}

## Solve for the inverse of the matrix restored by the "makeCacheMatrix" from the previous procedure.  
## The "cachesolve" should return the inverse from cache if the inverse is solved. 
cacheSolve <- function(x, ...) {
        
        ## Restore the matrix with the inverse of 'x'
        m <- x$getInverse()
        
        ## Restore the inverse if it is prepared
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        
}
  
## Retrieve the matrix from item
data <- x$get()
        
## Solve the incerse utilizing the multiplication of matrix 
m <- solve(data) %*% data
        
## Place the inverse to item
x$setInverse(m)
        
## Restore the matrix 
m
        
}

