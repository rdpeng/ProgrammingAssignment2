## The functions does an efficient job of calculating inverses of matrix.
## Since these operations are expensive, caching is done on the inverses
## so that they are not computed each time.
##
## CALLING SEQUENCE:
##		makeCacheMatrix() needs to be called first
##		cacheSolve() can be called any number of times afterwards
##
## EXAMPLE USAGE:
## mc <- makeCacheMatrix(matrix(c(2, 4, 8, 4, 5, 9, 8, 9, 3), nrow=3, ncol=3))
## cacheSolve(mc)
##
## OUTPUT IS SHOWN BELOW:
## calculating inverse for the first time...
##             [,1]       [,2]        [,3]
## [1,] -0.86842105  0.7894737 -0.05263158
## [2,]  0.78947368 -0.7631579  0.18421053
## [3,] -0.05263158  0.1842105 -0.07894737
##
## cacheSolve(mc) ## calling for the second time
##
## OUTPUT IS SHOWN BELOW:
## getting cached inverse
##             [,1]       [,2]        [,3]
## [1,] -0.86842105  0.7894737 -0.05263158
## [2,]  0.78947368 -0.7631579  0.18421053
## [3,] -0.05263158  0.1842105 -0.07894737

## FUNCTION: makeCacheMatrix
##
## ARGS:
##      x   - input matrix
##
## RETURNS:
##      A list of functions:
##          set - sets the matrix       USAGE: set(y)
##          get - returns the matrix    USAGE: get()
##          setInverse - sets the inverse of the matrix for caching
##                                      USAGE: setInverse(y)
##          getInverse - gets the inverse of the matrix
##                                      USAGE: getInverse()
##
##          All the get functions return NULL if the value is not set
##          All the set functions take a matrix as input
##
## The function uses lexical scoping to cache the values
##
makeCacheMatrix <- function(x = matrix())
{
    ## initialize variables in this lexical scope
    invertedMatrix <- NULL
    
    ## set function
    set <- function (y)
    {
        x <<- y
        invertedMatrix <<- NULL
    }
    
    ## get function
    get <- function()
    {
        x
    }
    
    ## setInverse function
    setInverse <- function (i)
    {
        invertedMatrix <<- i
    }
    
    ## getInverse function
    getInverse <- function()
    {
        invertedMatrix
    }
    
    ## return a list of functions
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Function: cacheSolve
##
## ARGS:
##      z   - cache matrix (created by a call to makeCacheMatrix)
##
## RETURNS:
##      Inverted matrix
##
## Please note the input matrix MUST be Invertible
##
## This function caches the result using helper function
## makeCacheMatrix upon first computation. After that,
## cached value is returned
##
cacheSolve <- function(z, ...)
{
    ## get the inverted matrix
    im <- z$getInverse()
    if (!is.null(im))
    {
        message("getting cached inverse")
        return (im)
    }
    
    ## need to compute inverse now
    data <- z$get()
    im <- solve(data, ...)
    
    ## set the inverse for caching 
    z$setInverse(im)
    
    ## return inverse
    message("calculating inverse for the first time...")
    im
}
