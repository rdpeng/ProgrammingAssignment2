## Assignment: Programming Assignment 2: Lexical Scoping
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## These functions use to cache the inverse of a matrix.


## PLEASE READ HOW TO RUN FUNCTIONS AT THE END


## makeCacheMatrix function will create and prepare the matrix to inverse
makeCacheMatrix <- function(x = matrix()) {
  r<-NULL
  set<-function(y)
  {
    x<<-y
    r<<-NULL
  }
  get<-function()x
  setinverse<-function(inv)r<<-inv
  getinverse<-function()r
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve function will inverse the pass matrix and catch the result
cacheSolve <- function(x, ...) {
    r<-x$getinverse()
   if(!is.null(r)) {
      message("getting cached data")
      return(r)
    }
    data <- x$get()
   r <- solve(data)
    x$setinverse(r)
    r  ## Return a matrix that is the inverse of 'x'
}



#############################
## HOW TO RUN THE FUNCTIONS##
#############################

#############
## SAMPLE 1
#############

##v<-matrix(rnorm(4), 2, 2)
##> v
##[,1]       [,2]
##[1,]  0.6255914 0.09793209
##[2,] -1.9789104 1.27368006

##> m = makeCacheMatrix(v)
##> m$get()
##[,1]       [,2]
##[1,]  0.6255914 0.09793209
##[2,] -1.9789104 1.27368006

##      FIRST RUN - NO CACH

##> cacheSolve(m)
##[,1]        [,2]
##[1,] 1.285764 -0.09886118
##[2,] 1.997685  0.63152640

##      SECOND RUN - CACH

##> cacheSolve(m)
##getting cached data
##[,1]        [,2]
##[1,] 1.285764 -0.09886118
##[2,] 1.997685  0.63152640

#############
## SAMPLE 2
#############

##v<-matrix(rnorm(16), 4, 4)
##> m = makeCacheMatrix(v)

##      FIRST RUN - NO CACH

##> cacheSolve(m)
##[,1]      [,2]       [,3]       [,4]
##[1,] -2.207721  3.504282 -0.1615105 -1.6226292
##[2,]  2.597254 -3.402954 -0.2058886  1.1160008
##[3,] -2.399837  2.467569 -0.2109272 -0.8854639
##[4,]  2.067509 -3.196237  0.5771765  0.9929895

##      SECOND RUN - CACH

##> cacheSolve(m)
##getting cached data
##[,1]      [,2]       [,3]       [,4]
##[1,] -2.207721  3.504282 -0.1615105 -1.6226292
##[2,]  2.597254 -3.402954 -0.2058886  1.1160008
##[3,] -2.399837  2.467569 -0.2109272 -0.8854639
##[4,]  2.067509 -3.196237  0.5771765  0.9929895


#######
##END##
#######