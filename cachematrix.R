## these two functions calculate the inverse of a matrix caching the result so
## each time we need to recalculate the value it give us the precomputed result

## in our first function, we will create a makeCacheMatrix function that will consist of
## four functions in a list to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # set the matrix (it is null at the beggining, but it changes when the user settles it)
  m<-NULL
  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  # Gets the matrix
  get<-function() x
  # set the inverse
  setmatrix<-function(solve) m<<- solve
  # Get the inverse
  getmatrix<-function() m
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  }


}


## we use this function to calculate the inverse of the matrix, but if it is already
## calculated it give us, the value that is stored in the computer. so the first time
## we rune it it calculate the inverse and the second time it give us the precalculated
## value

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  
  ##if there is another precomputed value it give it to us
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ##if not it takes the matrix, calculates the inverse and give us that inverse
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
 
## actually what i did was to copy the example from the class assignment and to change
## some text.
