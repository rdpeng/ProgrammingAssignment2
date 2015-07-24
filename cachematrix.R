## Put comments here that give an overall description of what your
## functions do

## these two functions will calculate the inverse of the matrix provided as parameter.
## Since this can be a costly computation, the inverse matrix will be cached, 
## so it can be retrieved faster thn doing the calculations again 
## The caching is only valid of course if the matrix is the same

## Write a short comment describing this function
## the first function will create the cache matrix with the SET and GET function
makeCacheMatrix <- function(x = matrix()) {
    myTempMatrix <- NULL
    ## The SET function add the matrix to the Cache
    set <- function(myMatrix){
        x <- myMatrix
        myTempMatrix <- NULL
    }
    ## the GET function retreives the matrix from th Cache
    get <- function() x
    
    ## the SETINVERSE function stores the 
    setInverse <- function(myInverse) myTempMatrix <<- myInverse  
    getInverse <- function() myTempMatrix
    
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function
## the second function will create the inverse matrix of the parameter x, 
## using the function above. First the cache is checked, to see if the result 
## was already calculated. Else the inverse is calculated and also stored in
## the cache..
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    myTempMatrix <- x$getInverse()
    if (!is.null(myTempMatrix)){
        message("getting cached data")
        return (myTempMatrix)
    }
    data <- x$get()
    myTempMatrix <- solve(data, ...)
    x$setInverse(myTempMatrix)
    myTempMatrix
}
