#RProgrammingAssignment2

##The below code is for cacheMatrix which is a function with x equal to matrix, j equals NULL, then there is a set equals function of y for x and y. Further the get function will act for the set and get inverse function as specified.

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
        x <<- y
        j <<- NULL
}
get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j 
        list(set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}

## The below code is for cacheSolve which is a function of x... and further the code will return an inverse of x matrix with the specified code where j gets the value of getInverse within x to execute the remaining written code.

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
        
j <- x$getInverse()
        if(!is.null(j)){
        message("getting cached data")
        return(j)
}
mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}
