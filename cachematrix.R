## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: creates a special "matrix" object that can cache its inverse
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the     	      inverse has already been calculated (and the matrix has not changed), then the cachesolve should 	 	      retrieve the inverse from the cache.


## Write a short comment describing this function

        # output of makeCacheMatrix is a list containing four functions to:
        # 1) set the value of the matrix
        # 2) get the value of the matrix
        # 3) set the value of the inverse of the matrix
        # 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	
	 	mat_inv <- NULL                                     # 1) sets default inverse of matrix value (mat_inv) to be undefined
        
        set <- function(y) {
                c <<- y   
                mat_inv  <<- NULL                           # 1) function that caches mat_inv to be undefined
        }
        
        get <- function() x                                 # 2) creates an object 'get' that is a function that retrieves the input matrix
        
        set_mat_inv <- function(solve) mat_inv <<- solve    # 3) creates function that will calculate mat_inv using solve
        
        get_mat_inv <- function() mat_inv                   # 4) creates a function that will retrieve the mat_inv value 
        
        list(set = set, get = get,
             set_mat_inv = set_mat_inv,
             get_mat_inv = get_mat_inv)                     # return value of makeCacheMatrix is a list of functions
        
}


## Write a short comment describing this function

# calculates the inverse of the matrix created with makeCacheMatrix
        # if inverse of matrix was already calculated in makeCacheMatrix, it retrieves cached value
        # if not, calculates the inverse of the matrix & caches newly calculated value of the inverse matrix
        

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          
        mat_inv <- x$get_mat_inv()      # x is vector output of makeCacheMatrix; sets mat_inv equal to output of get_mat_inv 
        
        if(!is.null(mat_inv)) {         # if there is a defined value of mat_inv cached,i.e. NOT equal to null, function returns cached mat_inv
                message("getting cached data")
                return(mat_inv)
        }
        
        data <- x$get()                # if no mat_inv value is found, sets the output matric of get() function in makeCacheMatrix as 'data'
        
        mat_inv <- solve(data, ...)    # calculates mat_inv value from output matrix
        
        x$set_mat_inv(mat_inv)         # inputs calculated mat_inv into setmean function of makeVector and caches it for later
        
        mat_inv                        # returns cached mat_inv, whether previously calculated in makeCacheMatrix or just calculated
}
