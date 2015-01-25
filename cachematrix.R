## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# initial input matrix for cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL    # set mInv to NULL (default if cacheSolve has not been called)
        set <- function( y ) {  # set matrix
                x <<- y # cache input matrix so cacheSolve can check if input changed
                mInv <<- NULL   # set inverse matrix in cacheSolve to NULL
        }
        get <- function( ) { x }
        setInverse <- function( solvedInv ) { mInv <<- solvedInv }
        getInverse <- function ( ) { mInv }
        list( set = set , get = get , setInverse = setInverse , getInverse = getInverse )
}


## Write a short comment describing this function
# return inverse that is either cached or solved (if not cached or null)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse( )    # get cached inverse matrix
        if( ! is.null( m ) ) {  # check if already cached
        #       if ( x$set( ) == x$get( ) ) {   # ck same input matrix???
                message( "getting cached data" )
                return( m )     # return cached inverse
        #       }
        }
        newMatrix <- x$get( )   # get new input matrix
        x$set( newMatrix )      # NB cache new input matrix
        m <- solve( newMatrix , ... )   # compute inverse matrix m
        x$setInverse( m )       # cache inverse matrix
        m       # return computed inverse        
}
