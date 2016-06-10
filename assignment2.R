makeCacheMatrix <- function(x = matrix()){    
 ##This function create a matrix (square matrix) and perform the following
  ##1.set the value of the matrix
  ##2.get the value of the matrix
  ##3.set the value of the inverse
  ##4.get the value of the inverse

    m <- NULL ## INVS is inverse of the matrix
   set <- function(y){
        x <<- y  # Assign a value using <<- to x and INVS different from the current environment
       m <<- NULL
     }
    get <- function() x ## get x the matrix
    setInverse <- function(solve) m<<- solve  ## set the inverse value using solve function

    getInverse <- function() m ## get the inverse value
   list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)  
}
## This function check for the matrix inverse if it was calculated using makeCacheMatrix, 
## and if not then it calculate it.


  cacheSolve <- function(x, ...) {
  ## get the inverse of the matrix
       m <- x$getInverse()                 
       if(!is.null(m)){                    
            message("getting cached data")     
           return(m)                        
          }
          ##if the inverse (m) is null then calculate the inverse
        data <- x$get()    ## get the matrix                 
        m <- solve(data, ...) ## calculate the inverse of the matrix using solve function              
        x$setInverse(m)       ## sets the value of the inverse in the cache using setinv
        return(m)           ##return thr inverse

  }
  
