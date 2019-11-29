## Put comments here that give an overall description of what your
## functions do

## Create a special matrix to set a matrix, get a matrix, set a inverse matrix, get a inverse matrix  
makeCacheMatrix <- function(x)  
	{
        
		x <- matrix(x,2,2)
		m <- NULL
        
set <- function(y) 
	{

		x <<- matrix(y,2,2)

		m <<- NULL
        
	}
        

get <- function() x
        

setinverse <- function(inverse) 
	      m <<- inverse
        

getinverse <- function() m
 
       
list(set = set, get = get,
 setinverse = setinverse, getinverse = getinverse)

	}


#Create a cache function to cache the inverse of a matrix and if not calculate the inverse of a matrix  
cacheSolve <- function(x, ...) 
	{
        
	m <- x$getinverse()
       
 		if(!is.null(m)) {
                
		message("getting cached data")
                
		return(m)
        
				}

        data <- x$get()
     
 	m <- solve(data, ...)
      
  	x$setinverse(m)
       
 	m
 
	}




