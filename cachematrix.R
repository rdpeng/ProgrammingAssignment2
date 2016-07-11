## the first function serves as a container for a cached matrix object storing a matrix
## and its inversion. the second function calculates the inversion of a given matrix and uses 
## an instance of the first function to store the calculated results.

## makeCacheMatrix creates a namespace for two variables, mat and inv_mat and returns 4 functions for
## setting and getting the values of those variables

makeCacheMatrix <- function() {
    
    mat     = matrix()
    inv_mat = matrix()
    
    get_matrix      = function() mat
    get_inv_matrix  = function() inv_mat
    set_matrix      = function( new_mat ){ mat <<- new_mat }
    set_inv_matrix  = function( new_inv_mat ){ inv_mat <<- new_inv_mat }
    
    return ( list( get_matrix     = get_matrix,
                   get_inv_matrix = get_inv_matrix,
                   set_matrix     = set_matrix,
                   set_inv_matrix = set_inv_matrix
                  )
           )

}


## cacheSolve calculates the inversion of a given matrix if inverted matrix not already stored in cache

cacheSolve <- function(cache_matrix, mat ) {
    
    if ( !identical( cache_matrix$get_matrix(), mat ) ){
        
        inv_mat = solve(mat)
        
        cache_matrix$set_matrix( mat)
        cache_matrix$set_inv_matrix( inv_mat )
        print('new inverted matrix calculated')
        
    }
    else{
        print('inverted matrix recovered from cache')
    }
    return ( cache_matrix$get_inv_matrix() )
}

# Code Testing the two functions

# instantiate cache
cache = makeCacheMatrix()

# generate two different matrices
mat1 = matrix(runif(9), 3, 3)
mat2 = matrix(runif(9), 3, 3)

# calculate inversions
cacheSolve (cache, mat1)
cacheSolve (cache, mat1)
cacheSolve (cache, mat2)


