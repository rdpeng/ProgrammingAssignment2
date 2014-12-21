
#  This function creates and matrix object, along with functions to set its value, get its 
#  value as well as getting and setting a cache of teh matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
         # Initialize a matrix with side of input matrix and fill with NAs
         cacheMatrix<- matrix(data=NA, nrow(x), ncol(x))
         
         # setter function to create a matrix & also a corresponding NA matrix
         # for the inverse matrix
         set <- function(y = matrix(data,rows,cols)){
                 x<<-y
                 cacheMatrix<<- matrix(data=NA, nrow(x), ncol(x))             
         }
         # function to "get" matrix
         get <- function() x
         
         # function to "set" cached matrix
         setcache <- function(cache) cacheMatrix <<- cache
         
         # function to "get" cached matrix
         getcache <- function() cacheMatrix
         list(set = set, get = get,
              setcache = setcache,
              getcache = getcache)
        
}


## This function returns the inverse of a makeCacheMatrix.
## If the inverse has been previously cached, the function returns the returned the cahed inverse
## Otherwise the inverse is computed, cached and returned.

cacheSolve <- function(x, ...) {      
        # get the cached matrix
        cacheMatrix <- x$getcache()
        
        # if the inverse has been computed, skip "solve" and just return the cached inverse 
        # and print message that cached matrix is being used.
        if (!is.na(cacheMatrix[1,1])) {
                 message("getting cached data")
        }
        else {               
        data <- x$get()
        cacheMatrix <- solve(data)
        x$setcache(cacheMatrix)
        }
        cacheMatrix
}
