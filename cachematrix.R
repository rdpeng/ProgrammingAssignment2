## The pair of functions take a matrix as input and gives the
## inverse as result. Also they stores the result so that there
## is no need of calculating again and again.

## Takes a matrix as argument and returns a list that contain four functions.

makeCacheMatrix <- function(x = matrix()) {
  Inv<-NULL
  set<-function(y){
    x<<-y
    Inv<<-NULL
  }
  get<-function() x
  setInv<-function(I) Inv<<-I
  getInv<-function() Inv
  list(setM=set,getM=get,setI=setInv,getI=getInv)

}


## This function checkes whether there is any cached result and if there is
## some old cached result then it gives it as result.

cacheSolve <- function(x, ...) {
  I<-x$getI()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data<-x$getM()
  I<-solve(data)
  x$setI(I)
  I
        ## Return a matrix that is the inverse of 'x'
}
