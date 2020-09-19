## The makeCacheMatrix function makes an object which can be used
## to store a matrix by setMatrix function. The cacheSolve function
## then is used to calculate the inverse of the matrix.

## The makeCacheMatrix function defines 4 functions: 
## 1. set function sets the given matrix in the object.
## 2. get function retrieves the matrix in the object.
## 3. setInv function sets the given inverse matrix in the object.
## 4. getInv function retrievs the inverse matrix in the object.
## The function returns a list containing these 4 functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(mat){
        x <<- mat
        inv <<- NULL
    }
    get <- function(){
        x
    }
    setInv <- function(calc_inv){
        inv <<- calc_inv
    }
    getInv <- function(){
        inv
    }
    list(set = set, get = get, 
        setInv = setInv, getInv = getInv)
}


## The cacheSolve function takes a object made by makeCacheMatrix
## function and checks if the inverse matrix is calculated or not.
## If the inverse is not calculated then it calculates the same and 
## store it in the given object. If it is calculated then it 
## retrieves the inverse matrix from the object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
    if(!is.null(inv)) {
        message("Getting Cached Inverse")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setInv(inv)
    inv
}
