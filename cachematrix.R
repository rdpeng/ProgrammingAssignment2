## Below is a combination of two functions 'makeCacheMatrix' 
## and 'cacheSolve'. Both these functions together help in 
## calculating the inverse of an invertible square matrix and 
## caching the value of the inverse of a particular matrix object
## once calculated, for future uses. This helps in saving on part 
## of calculating the inverse of the same matrix again and again,
## as the calculations are computationaly costly, specially when 
## matrix dimensions are large. Both these functions are very similar
## to the mean caching functions given in the example and work only
## for invertible square matrices.


## First function is makeCacheMatrix which creates an object in 
## the parent environment of the function, containing the matrix
## object passed to it as an argument (and for which inverse is 
## required), the inverse of the matrix (either Null or the value),
## and a list of four functions which can be called externally (like
## some of them are called in cacheSolve function).


makeCacheMatrix <- function(x = matrix()) {      # x initialised with matrix passed
        mat_inv <- NULL                          # mat_inv initialised with Null
        set <- function(y) {
                x <<- y          
                mat_inv <<- NULL                 # again similar initialisation done in case 'set' is called to change the matrix stored in x
        }
        get <- function() x                      # returns the matrix stored in x 
        setinv <- function(inv) mat_inv <<- inv  # sets the value of the inverse of the matrix in 'mat_inv' passed to it externally in 'inv'
        getinv <- function() mat_inv             # returns the inverse stored in mat_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)                    # this function returns a list of above four functions with their names so that they can be explicitly called externally in case required

}


## Second function is cacheSolve which either fetches and returns
## the cached value of the inverse of a matrix object or, in case,
## the matrix object is new, it calculates and returns the value
## of the inverse of the matrix. This function does this by first 
## retrieving the value stored in 'mat_inv' of the makeCacheMatrix
## object and then checking whether it is Null or not. If the value
## is not Null, then it straight away returns the retrived cached 
## value as the inverse and if the value is Null, it calculates the
## the inverse and returns it (while also setting it as the inverse
## value in the makeCacheMatrix object as well for that particular
## matrix)


cacheSolve <- function(x, ...) {                        # here, the argument 'x' is an object of the type makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinv()                           # the function retrieves the value of the inverse matrix using getinv function of the makeCacheMatrix
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }                                               # if the value of inverse matrix retrieved is not Null, then the same value is returned as the final value of the inverse of the matrix along with a message "getting cached data"
        data <- x$get()
        mat_inv <- solve(data, ...)                     # if the value retrieved above is Null, then this function first gets the value of the matrix in 'data' and then calculates its inverse and saves the result in mat_inv
        x$setinv(mat_inv)                               # function sets the value of the inverse for that particular matrix in the relevant makeCacheMatrix object for future caching if required 
        mat_inv                                         # it returns the value of the inverse of the matrix (cached or calculated, as the case may be)
}