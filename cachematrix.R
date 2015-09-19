## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(mat=matrix()) {
  i <- NULL
  set <- function(y) {              ## set matrix
    mat <<- y
    i <<- NULL
  }
  get <- function() mat             ## get matrix
  setinv <- function(inv) i <<- inv ## set inverse
  getinv <- function() i            ## get inverse
  list(set = set, get = get,        ## list of function
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
## it also check for all necessary condition for matrix inverse


cacheSolve <- function(mat, ...) {
  
  i <- mat$getinv()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- mat$get()
  
  if(sum(is.na(data))==0)          ## checking for NAs in the matrix
  {
    if(ncol(data)==nrow(data))     ## checking for square matrix
    {
      if(det(data)!=0)             ## checking for determinant of matrix (testing for sigularity of matrix)
      {
        i <- solve(data, ...)
      }
      else
      {
        print("Can't perform invers as the determinat of matrix is zero")
        i<-NaN
      }
    }
    else
    {
      print("Can't perform invers as it isn't a square matrix")
      i<-NaN
    }
  }
  else
  {
    print("Can't perform invers as their are NA(s) in matrix")
    i<-NaN
  }
  
  mat$setinv(i)
  i
}
