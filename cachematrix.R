## These functions were development as part of the assignment of R Programming Week 3

## This function will cache the matrix and its inverse inverse

makeCacheMatrix <- function(x = matrix()) {
        ##matrix inverse
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setInv<-function(inverse)inv<<-inverse
        getInv<-function() inv
        list(set=set,get=get,
             setInv=setInv,getInv=getInv)
}


## This function determines the inverse of a matrix, but before doing any calculation it first checks the cache

cacheSolve <- function(x, ...) {
        inv<-x$getInv()
        if(!is.null(inv)){
                message("Getting cached matrix data")
                return(inv)
        }
        ## verify if the matrix is square
        matrix<-x$get()
        if(dim(matrix)[1]==dim(matrix)[2]){
                inv<-solve(matrix)
                x$setInv(inv)
                inv
        }else {
                message("attempting to invert a non square matrix")
                }
}
