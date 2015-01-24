## two functions are defined: makeCacheMatrix is used to cache the inversion of a matrix
## cacheSolve is used to find the inverted matrix in the cache first. If not, the inverted matrix is calculated
## and saved in the cache for future use


## makeCacheMatrix defines four functions; two functions for the matrix, set_mat and get_mat
## two functions for the inverted matrix setmat_inv and getmat_inv
makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL        
        #set the matrix 
        set_mat <- function (y){
                x <<- y
                mat_inv <<- NULL                
        }
        
        #get the matrix
        get_mat <- function() x
        
        #sets the inverted matrix
        setmat_inv <- function(inverted_mat) mat_inv <- inverted_mat
        
        #gets the inverted matrix
        getmat_inv <- function() mat_inv
        
        #returns the list of the functions
        list(set_mat=set_mat,get_mat=get_mat,setmat_inv=setmat_inv,getmat_inv=getmat_inv)
        
}


## cacheSolve checks for the inverted matrix in the cache.
## if not available, it gets the matrix using get_mat, finds its inverse, set the inverted matrix using setmat_inv
## and finally returns the inverted matrix

cacheSolve <- function(x, ...) {
        mat_inv <- x$getmat_inv
        if(!is.null(mat_inv)){
                message("getting cached data")
                return(mat_inv)                
        }
        
        data <- x$get_mat()
        mat_inv <- solve(data,...)
        x$setmat_inv(mat_inv)
        #returning inverted matrix
        mat_inv
}
