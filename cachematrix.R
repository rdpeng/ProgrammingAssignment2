## cachematrix.R:
## Contains 2 functions, one of which creates an object that stores a matrix,
## and the other of which finds and caches that matrix's inverse


## makeCacheMatrix(x) :
## x is a matrix.
## Returns a list containing functions to get and set x and its inverse

makeCacheMatrix <- function( x = matrix() )
{
    i <- NULL # The inverse of matrix x, not yet set
    
    get_matrix  <- function(){ x }
    get_inverse <- function(){ i }
    
    set_matrix  <- function(m){ x <<- m }
    set_inverse <- function(m){ i <<- m }
    
    list( get_matrix = get_matrix, get_inverse = get_inverse,
          set_matrix = set_matrix, set_inverse = set_inverse )
}


## cacheSolve(x, ...) :
## x is a list created by the makeCacheMatrix function.
## Sets x's inverse if needed, and then returns x's inverse

cacheSolve <- function( x, ... )
{
    i <- x$get_inverse()
    if( !is.null(i) )
        message( "cached inverse used" )
    else
    {
        i <- solve( x$get_matrix() )
        x$set_inverse(i)
        message( "inverse calculated and cached" )
    }
    i
}
