## Put comments here that give an overall description of what your
## functions do all commits 

## Write a short comment describing this function
makecachematrix<-function(x=matrix()){
        ## this function creates a special "matrix" object which cache the inverse 

makeCacheMatrix <- function(x = matrix()) {
        ##define the argument with default mode of the "matrix"
        inv<-NULL## holds the value of matrix inverse
        set<-function(y){##defines the set functionto assign new
                x<<-y ## value of the matrix in the parent environment
                inv<<-NULL##if there is a new matrix ,reset inv to NULL
                }
        get<-function()x ## definethe get function -returns value of the matrix argument
        setinverse<-function(inverse)inv<<-inverse
        ##assigns value of the inverse in the parent environment
        getinverse<-function()inv## gets the value of inv when called
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
    }    
        
        


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting the cached data")
                return(inv)
                }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
}
