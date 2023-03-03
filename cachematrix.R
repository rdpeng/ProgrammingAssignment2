## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function()x
        set_inversematrix<-function(solve)s<<-solve
        get_inversematrix<-function() s
        list(set=set, get=get, set_inversematrix=set_inversematrix, get_inversematrix=get_inversematrix)
}



## Write a short comment describing this function

cacheSolve <- function(x) {
        s <- x$get_inversematrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$set_inversematrix(s)
        s
        
}
