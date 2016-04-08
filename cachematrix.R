## The assignment is to write an R function that is able to cache potentially
## time-consuming computations like matrix inversion


# The following function "makeCacheMatrix()" returns a list which
# is used an input to "cacheSolve()" fuction.

makeCacheMatrix <- function(x = matrix()) {
        # Input 'x': a square invertible matrix
        # Returns a list which is used as the input to cacheSolve()
        # containing functions to set & get the matrix, set & get its inverse
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The following fuction "cacheSolve()" computes the inverse of the "matrix"
# returned by makeCacheMatrix(). In case the value of inverted matrix has
# been calculated already, it will be retrieved from the cache

cacheSolve <- function(x, ...) {
        # Input 'x': output of makeCacheMatrix()
        # Returns a matrix that is the inverse of 'x'
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skip the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        return(inv)
}

# The following fuction "test()" takes in any invertible matrix and
#  calculates the inverse twice using above functions

test = function(mat){
        ## @mat: an invertible matrix
        
        temp = makeCacheMatrix(mat)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur1 = Sys.time() - start.time
        print(dur1)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur2 = Sys.time() - start.time
        print(dur2)
}

