## These two functions compute the inverse of any invertible matrix x that is given as argument to cacheSolve()
## In case the matrix has already been inverted the data is retrived from the cache with the message 'getting cache data'
## Otherwise the inverted matrix is calculated anew.

## makeCachematrix receives the matrix x and makes it the global environment argument m. Then it creates the empty 
## matrix im.

makeCacheMatrix <- function(x = matrix()) {
    ## Reset variables m, im, does not return any value
  m<<-x
  im <<- NULL
}


## cacheSolve verifies whether im is not null, and if the matrix x is identical to the last value of m. If both 
## conditions are met, then it retrieves the inverted matrix of m from the cache memory and does not recalculate.
## if any of the conditions above is not met, then the functions calls makeCacheMatrix and solves m. It returns 
## im, the inverted matrix of x.

cacheSolve <- function(x = matrix()) {
        ## Return a matrix that is the inverse of 'x'
  if((!is.null(im)) && (x==m))
    {
    message("getting cached data")
    return(im)
    }
    else{
      makeCacheMatrix(x)
      im<<-solve(m)
    } 
  im
}
