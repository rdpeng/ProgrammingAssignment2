## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

makeCacheMatrix <- function(xm = matrix()) {
invM <- NULL
setM <- function(ym) {
print("In setM.")
ym
invM <<- NULL
xm <<- ym
print("in setM")
xm
}
getM <- function() {
print("In getM.")
xm
}
setInvM <- function(im) {
print("In setInvM.")
invM <<- im
}
getInvM <- function() {
print(" In invM.")
invM
}
list(setM=setM, getM=getM, setInvM=setInvM, getInvM=getInvM)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(xm, ...) {
print("in CacheSolve, xm = ")
iM <- xm$getInvM()
if(!is.null(iM)) {
message("getting cached data")
return(iM)
}
data <- xm$getM()
iM <- solve(data, ...)
xm$setInvM(iM)
iM
 }

