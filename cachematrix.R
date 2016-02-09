#filename: cachematrix.R
#author  : I edited this for the use of assignment. Nothing much to add here though.
#date    :7th february

##function description

##The working of MakeCacheMatrix is to store the matrix in a different environment
## with the help of <<- operator
## for example by using a<-MakeCacheMatrix(m) can help to store the matrix m in 
## the environment linked to 'a' now.
## a$getmatrix will return the value of m now.
##ie it will help to create a cache memory to get the value if it was previously found.

## this is an example of the usage.
##m1<-matrix(sample(9),nrow = 3,ncol = 3)
##a<-makeCacheMatrix(m1)
##a$getmatrix()
##     [,1] [,2] [,3]
##[1,]    7    2    6
##[2,]    4    3    1
##[3,]    8    5    9
##> cacheSolve(a)
##[,1]      [,2]      [,3]
##[1,] 0.1428571 0.5000000 0.1666667
##[2,] 0.2500000 0.3333333 1.0000000
##[3,] 0.1250000 0.2000000 0.1111111
##> cacheSolve(a)
##[1] "returning cached inverse of matrix"
##[,1]      [,2]      [,3]
##[1,] 0.1428571 0.5000000 0.1666667
##[2,] 0.2500000 0.3333333 1.0000000
##[3,] 0.1250000 0.2000000 0.1111111


##    setmatrix will set the matrix to variable x
##    getmatrix will get the matrix value stored 
##    setinverse will cache the value of inverse 
##    getinverse will take the value of inverse stored if any.

makeCacheMatrix <- function(x = matrix()) {
        imatrix <- NULL
        
        setmatrix <-function(y){
          x<<-y
          imatrix<<-NULL
        }
        
        getmatrix <-function() x
        
        setinverse <-function(inverse) imatrix<<- inverse
        
        getinverse <- function()  imatrix 
  
        list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
  
}



## CacheSolve will set the inverse of the matrix to the cache., if the value of not already stored .
## It returns the value either way.
## the value of inverse is first stored to the variable imatrix
## if imatrix is not NUll ie there is a cache already available, print the message and return the 
## value. else compute the inverse and store the value as the cache for latter use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      imatrix<-x$getinverse()
      if(!is.null(imatrix)){
        print("returning cached inverse of matrix")
        return(imatrix)
      } 
      
      # this happens if the cache needs to be created ,where iverse is found and stored.
        data<-x$getmatrix()
        imatrix<- solve(imatrix)
        x$setinverse(imatrix)
        imatrix
        
      
}
