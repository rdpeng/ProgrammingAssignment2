## Function makeCacheMatrix sets the matrix, gets the matrix, sets the inverse matrix, 
##gets the inverse matrix.This function returns the functions mentioned above as list.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setmatrix<-function(inverse) i<<-inverse
  getmatrix<-function()i
  list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## Fuction cachesolve, checks whether the inverse matrix exist already. If yes,
## returns the inverse matrix from cache. If not, creates an inverse matrix
## based on the input data provided and sets the inverse matrix to chace. 

cacheSolve <- function(x, ...) {
        i<-x$getmatrix()
        if(!is.null(i)){
          message("getting cached inverse matrix")
          return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setmatrix(i)
        return(i)
}