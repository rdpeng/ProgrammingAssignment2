## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    solved<-NULL
    set<-function(y){
        x<<-y
        solved<<-NULL
    }
    get<-function() x
    setSolved<-function(solvedData){
        solved<<-solvedData
    }
    getSolved<-function(){
        solved
    }
    setSolved<-function(data){
        solved<<-data
    }
    list(set=set,get=get,getSolved=getSolved,setSolved=setSolved)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getSolved()
    if(!is.null(m)){
        print("get solved matrix from cache")
        return(m)
    }
    x$setSolved(solve(x$get()))
    x$getSolved()
}
