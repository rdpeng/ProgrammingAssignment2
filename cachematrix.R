## The functions are cacheing the invers of a matrix if it 
## is possible to generate. 
## 

## Making the cache object.
## 1. set the data in the object
## 2. get the the data from the object
## 3. setinverse set the inverse matrix
## 4. getinverse  get the inverse matrix from the object

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
      set <- function(y) {
                x <<- y
                i <<- NULL
        }
      
	get <- function() x
      setinverse <- function(inverse) i <<- inverse 
      getinverse <- function() i
      list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )


}


## This function is calculating the inverse of the matrix if 
## it is not in the cache.
## 1. get the inverse of the object x from the cache
## 2. if it is in the cache return it and write "getting cached data" message.
## 3. if it is not in the cache then calulate the inverse using solve(data, ...)
## 4. return the inverse
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()
      
	if(!is.null(m)) 
	{
         message("getting cached data")
         return(m)
      }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
