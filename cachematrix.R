## Put comments here that give an overall description of what your
## functions do

##Caching the Inverse of a Matrix using Funtions makeCacheMatrix and cacheSolve

## Write a short comment describing this function
##makeCacheMatrix = This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) 
{  
        ##This creates a list containing fuction to set and get the  value of Inverse matrix and matrix.
        invofmatrix<-NULL
        set<- function(y) 
        {
                x<<-y
                invofmatrix<<-NULL
           

}
        get<-function() x
                setinverse <- function(inverse) invofmatrix<<- inverse
                        getinverse<- function() invofmatrix
                                list(set=set,
                                     get=get,
                                     setInverse=setinverse,
                                     getInverse=getinverse)
                                }


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.                        

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        
        invofmatrix<-x$getinverse()
        if(!is.null(invofmatrix))
        {
                message("getting the cached data")
                return(invofmatrix)
                
}
        data<-x$get()
        invofmatrix<-solve(data,...)
        x$setinverse(invofmatrix)
        invofmatrix
        }
