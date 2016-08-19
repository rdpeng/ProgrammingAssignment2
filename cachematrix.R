## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
cache=NULL

setMatrix=function(newValue){
x<<-newValue
}

getMatrix=function(){
return (x)
}

setInverse=function(newInverse){
cache<<-newInverse
}

getInverse=function(){
return (cache)
}	

list(setMatrix=setMatrix,getMatrix=getMatrix,
setInverse=setInverse,getInverse=getInverse)


}


## Write a short comment describing this function

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x
        inverse=y$getInverse()

if(!is.null(inverse)){
message("getting cached data")
return (inverse)
}

else{
data<-y$getMatrix()
inverse=solve(data)
y$setInverse(inverse)
return (inverse)
}

}
