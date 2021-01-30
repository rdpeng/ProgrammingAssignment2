## Computing the inverse of objects can be computationally demanding and storing
## inverse objects in cache can therefore accelerate computing. These functions 
## create a special matrix object that can store its inverse in cache and check
## the cache if the inverse of a matrix has been stored.

## This function creates a special matrix object and stores its inverse in cache
## by creating a list containing functions to set and get the values of the matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {     # default argument is a matrix
        inv <- NULL                             # create empty object that will hold the inverse
        set <- function(y) {                    # define the function that sets value of matrix
                x <<- y                         # assign from external environment
                inv <<- NULL                    # empty inv if there is a new matrix
        }
        get <- function() x                     # define the function that gets inv matrix values
        set.inv <- function(inverse) inv <<- inverse # assign the inv values from parent environment
        get.inv <- function() inv               # get the values when called
        list(set = set, get = get,              # create list that contains the values;
             set.inv = set.inv,                 # name the elements in the list to
             get.inv = get.inv)                 # call them later
}

## This function checks whether the inverse of the argument exists in cache.
## If it does, it retrieves the cached values from the list created in the makeCacheMatrix function.
## If it doesn't, it uses the solve function to compute the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$get.inv()                      # Get the inverse values
        if(!is.null(inv)) {                     # If the inverse of matrix is cached
                message("getting cached data")  # let me know by printing a message.
                return(inv)                     # Return the cached inverse matrix
        }
        data <- x$get()                         # If inverse matrix is empty, get original matrix
        inv <- solve(data, ...)                 # and compute inverse with the solve function.
        x$set.inv(inv)                          # Set inverse matrix
        return(inv)                             # and return it.
}

set.seed(4)
mat <- matrix(rnorm(25), 5, 5)
mat.cache <- makeCacheMatrix(mat)
cacheSolve(mat.cache)
mat