## it saves in the cache the inverse of a matrix so
## R does not compute the inverse twice

## is a list containing a function, it sets the variables to work on

makeCacheMatrix <- function(x = matrix()) {
               # initialize inv variable
               inv <- NULL
               # define set function which superassigns two variables
               set <- function(y) {
                      x <<- y
                      inv <<- NULL
               }
               # defines get function, it returns the value of x
               get <- function()x
               # define getinv function, it superassigns the inverse 
               # value to the variable inv
               setinv <- function(inverse) inv <<- inverse
               # define getinv function which returns the value of inv
               getinv <- function() inv
               # the list that contains the functions describe above
               list(set = set,
                    get = get,
                    setinv = setinv,
                    getinv = getinv)

}


## this function checks if the inverse has been calculted

cacheSolve <- function(x, ...) {
    # assigns the variable inv the value obtained from the getinv
    inv = x$getinv()
    # if is not null then it will recall the value, prints it and ends
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    } 
    # if is not in the cache, it calculates the inverse
    # of the matrix
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
   } 
  

