1.# set the value of the vector
2. #get the value of the vector
3. #set the value of the mean
4. #get the value of the mean

makeVector <- function (x = numeric()){
  m <- NULL
  set <- function(y) {
       x <<- y
       m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean
       getmean = getmean)
}

cachmean <- function(x,...){
        m <- x$getmean()
        if(!is.null(m)){
          massage("getting cachhed data")
          return(m)
          data <- x$get()
          m<- mean(data,...)
          x$setmean(m)
          m
        }
}
 ASSIGNMENT: CACHING THE INVERSE OF A MATRIX

  write the following functions
 1. makeCachmatrix
 2. cachSolve
 
 makeCachematrix <- function(x = matrix()){
   j <- NULL
   set <- function(y){
     x <<-y
     j <<- NULL
   }
   get <- function(x)
     setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j
    list(set = set,get = get)
    setInverse =setInverse
    getInverse = getInverse
 }
 
 cachSolve < function(x, ...){
   j <- x$getInverse()
   if(!is.null(j)){
     massage("getting cached data")
     return(j)
   }
   mat <- x$get()
   j <- solve(mat,...)
   x$setInverse(j)
   J
 }
 
 