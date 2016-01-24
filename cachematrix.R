## The first of these two functions create a special matrix and that stores a matrix and cache's its inverse. 
## The second function calcuates the inverse of a passed matrix but before calculating the inverse it checks to see
## if the inverse to that same matrix exists in cache.

## makeCacheMatrix creates a special matrix, which is really a list containing a function to 1. set the value of the matrix
## 2. get the value of the matrix 3. set the value of the inverse 4. get the value of the inverse

makeCacheMatrix <- function(a = matrix()) {
        n <- NULL
        aset <- function(b=matrix()) {
                a <<- b
                n <<- NULL
        }
        aget <- function() a
        asetinverse <- function(inv=matrix()) n <<- inv
        agetinverse <- function() n
        list( aset = aset, aget = aget,
              asetinverse = asetinverse,
              agetinverse = agetinverse)
}

## This function calculates the inverse of the special matrix already created but first it checks to see if the inverse already
## exists and that the matrix is the same. If the inverse already exists the new matrix is the same as the cached inverse
## then the inverse is returned from cache and not calculated.

cacheSolve <- function(a=matrix(), ...) {
        md <- a[aget()]
	n <- a[agetinverse()]
	if (!is.null(n) && identical(md,n)) {
                 message ("getting cached data")
                 return(n)
        }
        n <- solve(a, ...)
        a[asetinverse(n)]
	a[aset] <- a
        n	
}
