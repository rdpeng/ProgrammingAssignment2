## Put comments here that give an overall description of what your
## functions do
## our aim is to write a pair of function,namely,"makeCacheMatrix","cacheSolve" that cache the inverse of a matrix


## Write a short comment describing this function
##makeCacheMatrix is a function which creates a special"matrix"object that cache its inverse for the input (wich is an invertible square matrix)
makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- functiotion(y) {
                x < <-y
                inv <<-NULL 
                }
                get <-function()x
                setinv<-function(inverse)inv <<- inverse
                getinv<- function()invlist(set=set, get=get,setinv=setinv,getinv=getinv)
}
## Write a short comment describing this function
## cachesolve is a function wich computes the inverse of the special"matrix"
## returned by makecachematrix above.if the inverse has already been calculated ,then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv(
                if(!is.null(inv)){
                        message("getting cached result")
                        return(inv)
                        }
                        data<-x$get(
                                inv<-solve(data,...)
                                x$sentinv(inv)
                                inv       
}

