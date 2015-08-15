## makeCacheMatrix and cacheSolve enable to save time in getting an inverse of 
## a matrix.
## The inverse is stored in cache, and if the inverse is needed more than once,
## and if the matrix hasn't changed in the meantime - the inverse will be drawn
## from the cache instead of being calculated again.

## makeCacheMatrix defines 4 functions:
## 1. set - accepts a matrix as an argument and puts it in variable x.
## 2. get - returns the variable x on demand.
## 3. setinv - accepts a matrix that is the inverse of x and put it in variable inv.
## 4. getinv - returns the variable inv on demand.
## The 4 functions are returned by makeCacheMatrix as a list.

## Usage: var <- makeCacheMatrix()
##        var$set(matrix)
##        
      
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(mat) inv <<- mat
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve yields the inverse of a matrix y by computing it, or by finding
## its value if it was calcultated before, and the matrix y hasn't changed.
## It accepts 2 arguments: x and y.
##              x is a list that was produced by "makeCacheMatrix" and
##                contains the values of y, and its inverse, as they were at
##                the time of the calculation.
##              y is the matrix with its current values.
## cacheSolve checks first that y hasn't changed by comparing it to the values
## stored in x.
## If y is unchanged cacheSolve checks if an inverse is contained in x.
## If yes - it returns the pre-calculated inverse.
## If not - it calculates the inverse, stores it in x and returns it.
## If y has changed cacheSolve updates x with the new values, calculates a new
## inverse, updates x with the new inverse and returns the new inverse.


cacheSolve <- function(x, y, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(identical(x$get(), y)){  # Check that matrix has not changed.
        inv <- x$getinv()
        if(!is.null(inv)){      # Check if inverse stored in cache.
            message("getting cached data")
            return(inv)
        }
        data <- x$get()         # If not:
        inv <- solve(data)      # Calculate new inverse.
        x$setinv(inv)
        return(inv)
    }
    else{                       # If matrix has changed...
        x$set(y)                # Store new matrix
        inv <- solve(y)         # Calculate its inverse
        x$setinv(inv)           # Store the new inverse
        return(inv)             # And return it.
    }
}

## Example:
size <- 3
mat <- matrix(sample(size^2), size, size) # construct matrix of given size
s <- makeCacheMatrix()  #   Construct the list
s$set(mat)              #   Set the matrix in the list
s$get()                 #   View the matrix
cacheSolve(s, mat)      #   Get the inverse (It will be calculated)
cacheSolve(s, mat)      #   Get the inverse (It will be taken from cache)
