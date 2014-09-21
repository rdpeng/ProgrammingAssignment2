## makeCacheMatrix is a function containing four other function, which can set the matrix,
## read the matrix(get function), set the inverse of this square matrix and read the value of 
## inverse (getinver function)

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {x}
        setinver <- function(i) { inverse <<- i}
        getinver <- function(){inverse}
        list(set=set,get=get,setinver=setinver,getinver=getinver)

}


## cacheSolve is used to calculate the inverse of given matrix, and it has antoher function that if 
## the matrix's inverse has been calculated, it will just read it from cahce.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver<-x$getinver()
        if (!is.null(inver)){
                message("getting cache data")
                return(inver)
        }
        matrix<-x$get()
        inver<-solve(matrix,...)
        x$setinver(inver)
        inver
}
