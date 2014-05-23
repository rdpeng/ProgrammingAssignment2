## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        maxt<-NULL
        setma<-function(y){
                x<<-y
                maxt<<-NULL
                }
        getma<-function() x
        
        setmaxt<-function(maxt) maxt<<-maxt
        getmaxt<-function() maxt
        
        list(setma=setma,getma=getma,setmaxt=setmaxt,getmaxt=getmaxt)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        maxt<-x$getmaxt()
        if(!is.null(maxt)){
                message("getting cache matrix")
                return(maxt)
                }
        tt<-x$getma()
        maxt<-solve(tt)
        x$setmaxt(maxt)
        maxt
}

ab<-matrix(c(200,0,5,2,1,6,3,4,0),3,3)
ab
aa<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
aa
testa<-makeCacheMatrix(aa)
cacheSolve(testa)
testb<-makeCacheMatrix(ab)
cacheSolve(testb)
