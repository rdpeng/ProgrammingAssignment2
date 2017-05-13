

## The function makeCacheMatrix requires a input of a matrix, and it returns a list of command used in the function cachesolve/
## The fucntion cachesolv requries a input of the output of function makeCacheMatrix
## cacheSolve returns the inverse matrix of hte matrix input to the makecacheMatrix and stroe it in the ourput of makecacheMatrix
## If the inverse matrix has been cached, it will returns message 'getting cached data' and immediately give result without calculate.

## input a matrix and we will get a list for further use

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        get<-function()x
        getinv<-function()inv
        setinv<-function(i)inv<<-i
        list(set=set
             get=get,
             getinv=getinv,
             setinv=setinv)

}


## input the output of the function above and get the invesrse matrix of the input of the funciton above

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message('getting cahced data')
                inv
        }
        target_matrix<-x$get()
        i<-solve(target_matrix)
        x$setinv(i)
        i
}
