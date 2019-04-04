## The following two functions are able to cache the inverse of a given square invertible matrix, so that the inverse is computed
## only once, and every time you need it, its value will be retrieved from the cached data, rather than re-compute it again.
## If a new matrix is given, its inverse will be computed de novo, and its value will overwrite the previously cached data.

## The first function creates a special matrix object, which is actually a list of four functions: the "set" function sets the 
## value of the square matrix given by the user as the argument of the main function, the "get" function gets the value of that
## matrix, the "setinv" function sets the value of the inverse of the matrix, and the "getinv" function gets the value of the 
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
x<<-y
inv<<-NULL
}
get<-function()x
setinv<-function(inverse)inv<<-inverse
getinv<-function()inv
list(get=get,set=set,setinv=setinv,getinv=getinv)
}


## The second function prints tha value of the inverse of the matrix object created by the previous one (it is provided as 
## the argument of this function). If the inverse has already been computed, its value is retrieved from the cached data, 
## otherwise it is calculated de novo and its value will overwrite the previously cached data.

cacheSolve <- function(x, ...) {
I<-x$getinv()
if (!is.null(I)) {
message("getting cached result")
return(I)
}
mat<-x$get()
I<-solve(mat,...)
x$setinv(I)
print(I)      
}
