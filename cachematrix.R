## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix takes a matrix as an argument and returns a list of 3 functions (described below)
#getData tells us the input matrix
#getCache retrives the cached matrix, which is stored as output in the environment function makeCacheMatrix
#setCache takes one argument and assigns that variable to the output variable in the parent environment (in the function makeCacheMatrix), using the <<- operator
#We can think of makeCacheMatrix as a "special matrix" the corresponds to the original input matrix

makeCacheMatrix <- function(x = matrix()) {
    output <- NULL #This is the cached outcome that getCache retrieves from and that setCache writes to
    getData <- function() x #This returns the original input matrix which is taken from the argument of makeCacheMatrix
    getCache <- function() output
    setCache <- function (solved){
        output <<- solved #The <<- operator assigns the input argument to the output in the parent environment
    }
    return(list(getData = getData,getCache = getCache, setCache = setCache)) #A list of 3 functions is returned
}

## Write a short comment describing this function
#cacheSolve first checks if the cache for that "special matrix" is empty or not. If it is not, then it returns the cache directly without further calculations
#If the cache is empty, then it calculates the inverse, stores it in the cache and also returns it. 

cacheSolve <- function(x, ...) {
    currentCache <- x$getCache()
    if (!is.null(currentCache) ){
        print ("getting cached data")
        return(currentCache)
    } else {
        inversed <- solve(x$getData())
        x$setCache(inversed) #calls the setCache function 
        #recall setCache was defined in makeCacheMatrix, so lexical scoping demands that the <<- operator assigns inversed to output (which is in makeCacheMatrix)
        return(inversed)
    }
        ## Return a matrix that is the inverse of 'x'
}



