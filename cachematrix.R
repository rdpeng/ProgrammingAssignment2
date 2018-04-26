## Steven Immel
## 04/26/2018
## This program allows the user to create a matrix using the 
## set function created in the makeCacheMatrix function. This
## matrix, if not previously inverted, is inverted by the 
## cacheSolve function and saved in cache. If the inverse has
## already been solved cacheSolve will retrieve the result 
## from the cache. This is intended to save time.

## The makeCachematrix fucntion sets and gets a matrix prior 
## to it being inverted. This function also saves and recovers 
## the inverted matrix built in the cacheSolve function

makeCacheMatrix <- function(mtx = numeric()) {
  m <- NULL
  set <- function(y,l=1,w=1) {
    test<-sqrt(length(y))%%1
    if(class(y)=="matrix") mtx<<-y
    else if(class(y)=="integer" | class(y)=="numeric"){
      if (test==0){
        w<-sqrt(length(y))
        mtx <<- matrix(y,w,w)}
      if (test>0){
        print("There is an incorrect number of values")}
      else if(class(y)=="data.frame"){
        print("Does not work with the data frame class")
      }
      else if(class(y)=="list"){
        print("Does not work with the list class")
      }
    }
    m <<- NULL
  }
  get <- function() mtx
  setmatrix <- function(mtrx) m <<- mtrx
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)}


## The cacheSolve function detects if a matrix has been
## inverted. If it has not, the function will invert and
## save the inverted matrix. If it has inverted, the 
## matrix it will recover it from the cache.

cacheSolve <- function(mtx, ...) {
  m <- mtx$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)}
  data <- mtx$get()
  m <- solve(data, ...)
  mtx$setmatrix(m)
  m}
