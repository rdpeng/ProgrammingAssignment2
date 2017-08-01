## Hi! Below is the description of these two functions!

## This pair of functions is used to cache the inverse of a matix given;
## it is used to avoid a waste of time calculating the same inversion twice;
## it works in the following order:
## 1. sourse these two functions into your console;
## 2. store the result of makeCacheMatrix function, whose argument is the martix created to be inversed;
##    e.g.   x<-makeCacheMatrix(matrix(1:4,2,2))   (In here, the to-be-inversed matrix is matrix(1:4,2,2).)
## 3. call cacheSolve function on the result you just stored to calculate the inversion;
##    e.g.   cacheSolve(x)
## 4. the first time you cacheSolve this result, only the inversion of this matrix will be returned, without the message;
## 5. now if you call cacheSolve on this result again, both the inversion of this matrix and a message written "getting cached data: the inverse of the matrix" will be returned.
##    This means your specific matrix's inversion is successfully cached and the system can simply recall the cached data.

## MAKECACHEMATRIX function, or the first function, is created to cache/store the data, so that the second function can call on it;

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL     ##initialize the value of inverse
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinverse<- function(solve) inverse<<- solve
  getinverse<- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ##these variables will be used in the second function, somehow connect these two functions
}


## CACHESOLVE function is designed to solve the inversion of the matrix given, it will either inverse the matrix or load the current cache of this inversion.

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){  
    ##if the inversion is cached, which means the inverse value is not null, the code here will load the existed data
    message("getting cached data: the inverse of the matrix")
    return(inverse)
  }
    data<-x$get()  ##otherwise, the code here calculates the inversion of the matrix given
    inverse<-solve(data,...)
    x$setinverse(inverse)
    inverse
}
