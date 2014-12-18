## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
s <- NULL #cache (set matrix to NULL
  set <- function(y) { #Passing a square matrix 
    x <<- y #Matrix assigned to a variable (x) in makeCacheMatrix 
    s <<- NULL #if set() get new matrix, clear previous cache 
}
get <- function() x #get() subfunction returns matrix stored in x
  setinv <- function(solve) s <<- solve #inverse matrix is passed to (solve)
  #and stored in the parent environment
  getinv <- function() s #Returns cache
  list(set = set, get = get, #list of all subfunctions
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
s2 <- x$getinv() #call makeCacheMatrix using getinv() subfunction, which
#returns the cache "s" and stores it in a new variable "s2".
  if(!is.null(s2)) { #checks if the returned cache is empty (!is.null(s2).
    return(s2) #and the content of cache is returned.
}
matrix <- x$get() #else Matrix passed with get()
  inv_matrix <- solve(matrix, ...) #Inverse using the solve() function.
  x$setinv(inv_matrix) #Pass inverse matrix to setinv() which stores it to "s" (cache).
  inv_matrix #Inverse matrix Returned.
}
