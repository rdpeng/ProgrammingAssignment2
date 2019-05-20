## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { #define the default value of the matrix
    inv_mat <- NULL                      #create initial value and assign null to it 
    set_mat <- function(y) {            # set the matrix in parent environment
        x <<- y                         
        inv_mat <<- NULL                    #in case that it is a new matrix then reset inv_mat to null
    }
    get_mat <- function() x               #no input variable. get the value of matrix in parent environment

    set_inv <- function(inv) inv_mat <<- inv  # sets invertible matrix value as input
    get_inv <- function() inv_mat             ## gets inv_mat value
    list( set_mat =  set_mat, get_mat = get_mat, set_inv = set_inv,  get_inv = get_inv)  ## set list in order for using $ operator
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv_mat <- x$get_inv()
         if(!is.null(inv_mat)) {                             #run when matrix is not null
                message("obtain cached invertible matrix")   #shows message: obtain cached invertible matrix
                 return(inv_mat)                             #return the invertible matrix
          }

  
         mat_data <- x$get_mat()                    # run when invertible matrix is NULL 
         inv_mat <- solve(mat_data, ...)            #get inverse the matrix
         x$set_inv(inv_mat)                         #set the invertible matrix 
          return(inv_matrix)                        #return the invertible matrix
}
