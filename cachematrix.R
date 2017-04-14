## (1)  Create a function that contains a matrix and its calcuated inverse
## (2) Create a function that calculates the function matrix's inverse
## A "Class" function that contains a matrix and its calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        s<- NULL
        set<-function(y){
        x<<- y
        s<<- NULL
        }
        get<-function() x
        setSolve<-function(val) s<<-val
        getSolve<-function() s
        list(set=set,get=get,setSolve=setSolve, getSolve=getSolve)
}


## A function that calculates a function matrix's inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'     
        s<- x$getSolve()
        if(!is.null(s)){
                message('Get cached matrix inverse')
                return (s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setSolve(s)
        s
}
