## MakeCacheMatrix returns a string with fuctions as elements 
## the list elements are in turn inputs to the cacheSolve function
## for the purpose of computing the inverse of a matrix X which is the formal variable of the present function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                m <<- NULL
                x <<- y
                
        }
        getMatrix <- function() x
        setMatrixInverse <- function(solve)
                m <<- solve
        getMatrixInv <- function() m
        print(m)
        list(set = setMatrix, get = getMatrix, setInverse = setMatrixInverse, getInverse = getMatrixInv)
}


## The function takes X and checks if the inverse was already computed
## If so, returns the cached inverse matrix. If not already computed, it calculates X´s inverse
## and returns it. If the inverse were already available its value is obtained from the cached value
## thus avoiding going through the calculation process again

cacheSolve <- function(x, ...){
        ## The function calculates the inverse of the matrix generated as the output of makeCacheMatrix (see above)
        ## First check if the inverse was alredy calculated
        m <- x$getInverse()
        if(!is.null(m)) {
                ## If an inverse was already computed do not compute ago but get the value and get out
                message("getting cached data")
                return(m)
        }
        ## If an inverse was not available already, first get the matrix
        data <- x$get()
        ## Then compute the inverse
        m <- solve(data, ...)
        ## and finally store the value
        x$setInverse(m)
        m 
        ## Returns a matrix that is the inverse of 'x'
}
