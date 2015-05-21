# Put comments here that give an overall description of what your
# functions do
# Write a short comment describing this function
#get function is used to assign the matrix
#setinverse and getinverse functions is used to cache the inverse function
#The setinverse function the value of i is added to the parent environment by doing that getinverse function can 
#pull the inverse of the matrix without calculating
makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x <<- y
                i <<- NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function()  i
        
        list(set = set,get=get,setinverse=setinverse,
             getinverse=getinverse)
        
}


## Write a short comment describing this function
#look for the inverse of the x matrix with the getinverse function(getinverse function can be 
#functional because the i value is assigned with <<- in the makeCachematrix)
#if it is null then the inverse will be calculated if it is calculated before than the data will be cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                message("cached")
                return(i)
        }
        data<-x$get()
        i<-solve(data,,...)
        x$setinverse(i)
        i
}