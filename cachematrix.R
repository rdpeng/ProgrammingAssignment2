# This code calculates inverse of a matrix for the first time (for a new matrix).
# If inverse of same matrix is computed again, it returns that value from the cache
# instead of calculating again.


## This function creates a matrix object which takes original matrix (also keeps it)
# and caches the inverse of matrix. Inverse is not calculated in this function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #Initialize a variable which will store cached value of inverse matrix
    #Function to set new data
        setData <- function(newData) {
                x <<- newData
                inv <<- NULL  #if new data received, set inverse to NULL
        }
        getMatrix <- function() x #Return the matrix as it is; it can only be exposed via a function
    #Assign calculated value of matrix inverse to the variable declared above
    #Using super assignment because variable inv does not exist inside the function setCache
        setCache <- function(matrixInv) inv <<- matrixInv
        getInv <- function() inv #Return cached value of inverse, can be NULL too

        list(getInv = getInv, getMatrix = getMatrix,
             setCache = setCache,setData = setData)
}


## This function instantiates the object makeCacheMatrix. Calculates inverse for the first
# time and uses makeCacheMatrix to cache the inverse of matrix.

cacheSolve <- function(y, ...) {
    ## Return a matrix that is the inverse of 'x'
    #Get Inverse matrix from makeCacheMatrix function
        rinv <- y$getInv()
        if (!is.null(rinv)) {
                message("Getting from Cache")
                return(rinv)
        }
        message("Calculating afresh")
    #If inverse matrix is NULL then calculate inverse
        data <- y$getMatrix()  #First get original matrix from makeCacheMatrix function
        rinv <- solve(data)  #Solve for inverse of matrix
        y$setCache(rinv)  #Store the calculated value of matrix inverse in cache
        rinv  #Print inverse of matrix
}
