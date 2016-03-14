## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix generates a new matrix if x is invalid and 
## and also caches the Inverse value

makeCacheMatrix <- function(x = matrix()) {
# check it is a square matrix, if not make new
  if(!nrow(x) == ncol(x)){ 
		x <- matrix(rnorm(25),5,5) 
		}
   newInverse <<- cacheSolve(x)
}


## Write a short comment describing this function
## mx is the global cache for x. first compare global mx with x
## if x and mx are different, generate Inverse and save to Cache and return
## if x and mx are same, then make sure global newInvers exists. if Exists return from cache, 
## else generate newInverse and save it to cache and return
## final else statement also generates newInverse and caches..

## cacheSolve can be called independently and note required to call
## from makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# assign global memory mx to oldmx
  if(exists('mx')){
    oldmx <- mx}
   else{
# if global memory of mx doesn't exist, generate newInverse and save to Cache
      mx <<- x
      newInverse <<- solve(x, LINPACK = FALSE)
      return(newInverse) }

# check if x and oldmx are same
    if( is.matrix(x) && is.matrix(oldmx) && dim(x) == 
		dim(oldmx) && all(x == oldmx))
         {  
# if the Inverse exists, then return the same..
		if (exists('newInverse'))
                 {	  				  	
			print("cache")
               	return(newInverse) 
                 }
		else {
# if Inverse does not exist, then create newInverse and cache it!
	            newInverse <<- solve(x, LINPACK = FALSE)
                 }
          } 
      else
        {
# if the old matrix and x are different, then cache the new Inverse for x
          newInverse <<- solve(x, LINPACK = FALSE)
        }
# assign x to global memory 'mx'
  mx <<- x 
  print("new")
  return(newInverse)         
}
