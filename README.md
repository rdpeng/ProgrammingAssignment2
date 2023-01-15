# Assignment-2
Matrix CACHE
#this is used to get the cache data
cachesolve <-function(x,....)#gets the cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){ #checking whether inverse is null
    message ("getting cached data!")
    return inverse  #return inverse value
  }
  data<-x$get()
  inv<-solve(data,...)  #calculate inverse value
  x$setinv(inv) 
  inv ## return the matrix that is the inverse of 'x'
}

f<-makecachematrix(matrix(1:8,2,4))
f$get()
      [.1] [.2] [.3] [.4]
[1.]   1     3    5    7
[2.]   2     4    6    8


##functions do
##there are two functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix cosist of set,gert,setinv,getinv
##library(mass)is used to calculate inverse for non-squared as well as squared matrices.

makeCacheMatrix <- function(x = martrix()) {
  inv<- NULL #initializing inverse as null
  set <- function(y) {
    x <<- y
    inv<<- NULL
    set<-function(y){
      x<<-y
      inv<<-null
      }
  }
  get <- function() x #function to get matrix x
  setinv <- function(inverse) inv<<- inverse
  getinv <- function(){
    inver<-ginv(x)
    inver%*%x  #function to obtain inverse of the matrix
  } 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##this is used to get the cache data

cachesolve <- function(x, ...)##gets cache data
  {
  inv<- x$getinv()
  if(!is.null(inv)) { ##checking whether inverse is null
    message("getting cached data")
    return(inv)   ##return inverse value.
  }
  data <- x$get()
  m <- solve(data, ...) ##calculate the inverse value
  x$setmean(inv)
  inv ##return a matrix that is inverse of 'x'
}
