# Matrix inversion is usually a costly computation and there may be some benefit

# to caching the inverse of a matrix rather than compute it repeatedly. The

# following two functions are used to cache the inverse of a matrix.




# makeCacheMatrix creates a list containing a function to

# 1. set the value of the matrix

# 2. get the value of the matrix

# 3. set the value of inverse of the matrix

# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
# makeCacheMatrix creates a list containing a function to

inv<- NULL
# sets inv as undefined for now
sl <- function(y){
	x<<-y
	inv<<- NULL
}
get <- function()x
setInverse <- function(invVal) {

    cachedInv <<- invVal 

    return(cachedInv)

  }
getInverse  <- function() {cachedInv

  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}

#assessing inv
#creates a list
}

# The next function checks if the matrix has already been computed, if it has, it print the stored value, else it calculates the inverse of the matrix.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x = matrix(), ...) {
	 inv <- x$getinverse()

    if(!is.null(inv)) {

        message("getting cached data.")
        #checking if data is already cached

        return(inv)

    }

        data<-x$get()
        inv<- solve(data)
        x$setinverse(inv)
        #outputting inverse
        inv
}
