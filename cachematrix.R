
#id:nitesh-mscit
#week-3- peer review assignment

#makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.
#cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). If the inverse has already been 
#calculated and the matrix has not changed, it’ll retrieves the inverse from the cache directly.

######################################################################################
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inve <<- NULL
        }
        get = function() x
        setinv = function(inverse) inve <<- inverse 
        getinv = function() inve
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

######################################################################################
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
   
cacheSolve <- function(x, ...) {
        inve = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inve)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inve)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inve = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inve)
        
        return(inve)
}

######################################################################################
