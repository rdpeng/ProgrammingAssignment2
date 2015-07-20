##These 2 functions calculate the inverse of a given matrix x
#By putting the inverse in cache, we save computational time
#by not computing the inverse if we already did previously


##First, makeCacheMatrix take as argument a matrix x
#It returns a list containing 4 values : 
#  1 set the value of the matrix
#  2 get the value of the matrix
#  3 set the value of the inverse (we suppose the matrix invertible)
#  4 get the value of the inverse
  
makeCacheMatrix <- function(x = matrix()) {
                inverse <- NULL
                set <- function(y){
                  x<<-y
                  inverse <<- NULL
                }
                get <- function() x
                setinverse<-function(inverse) inverse<<- solve(x)
                getinverse<-function() inverse
                list(set=set,get=get,
                     setinverse=setinverse,getinverse=getinverse)
}


#Then cachesolve will return the inverse of the matrix x
#But first it checks if the inverse has already been computed 
#(and thus will not be computed again, to save computing time)
#If the inverse hasn't been computed yet, cachesolve compute it, put it in cache and return it

cacheSolve <- function(x, ...) {
##First check if we already have the inverse
            inverse<- x$getinverse()
          if(!is.null(inverse)){
              message("getting cached data")
              return(inverse)
          }
        ##If we don't have the inverse, then compute it and put it in cache
          xcache<-x$get()
          inverse<-solve(xcache)
          x$setinverse(inverse)
## Return a matrix that is the inverse of 'x'
          return(inverse)
}

