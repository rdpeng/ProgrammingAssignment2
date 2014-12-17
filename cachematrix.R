## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
s <- NULL #cache (set matrix to NULL
  set <- function(y) { #Passing a square matrix 
    x <<- y #Matrix is assigned to a variable (x) in makeCacheMatrix 
    s <<- NULL #if set() get a new matrix, clear previous cache 
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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
s2 <- x$getinv() #call makeCacheMatrix using getinv() subfunction, which
#returns the content of cache "s" and stores it in a new variable "s2".
  if(!is.null(s2)) { #checks if the returned cache has anything in it (!is.null(s2).
    message("getting cached Inverse Matrix") #If so, prints "getting cached Inverse Matrix"
    return(s2) #and the content of cache is returned.
}
matrix <- x$get() #else we get the matrix passed with get()
  inv_matrix <- solve(matrix, ...) #and we inverse it using the solve() function.
  x$setinv(inv_matrix) #we pass the inverse matrix to setinv() which stores it to "s" (cache).
  inv_matrix #inverse matrix is returned.
}
