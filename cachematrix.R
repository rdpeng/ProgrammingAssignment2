## ALMUDENA DEVESA PEIRO

## PROGRAMMING ASSIGNMENT 2: caching the inverse of a matrix 

## this function creates a special "matrix", which is a list that contains a function to set and get the value of
## a matrix and to set and get the value of its inverse. 

makeCacheMatrix <- function(x = matrix()) {         #creates a matrix
  imat <- NULL
  set <- function(y) {                        #stores the matrix in a different environment
    x <<- y
    imat <<- NULL                       #imat is the inverse of the x matrix, not yet calculated
  }
  get <- function() x                           #funciton that takes the matrix x as an argument          
  setimat <- function(solve) imat <<- solve     #sets the inverse of the x matrix with solve (only for squared matrices. Other dimensions see below)
  getimat <- function() imat                    #gets the inverse of the matrix x
  list(set = set, get = get,                    #list of all the above functions
       setimat = setimat,
       getimat = getimat)
}

# this secong function calculates the inverse of the special "matrix" creared with 
## the above makeCacheMatrix function. If the inverse of that particular matrix has already been calculated, it gets the inverse of the matrix and skips 
## the calculation. If not it calculates it and sets its value in the cache via the setimat function

cacheSolve <- function(x, ...) {                    #function that takes the x matrix created above as an argument
  imat <- x$getimat()                         #it gets the inverse of the matrix from the cache if it was already 
  if(!is.null(imat)) {                        #calculated(imat is not na (not missing))
    message("getting cached data")
    return(imat)                        #returns imat(the inverse of the matrix x)
  }
  data <- x$get()                             #if not calculated 
  imat <- solve(data, ...)                    #gets the inverse of the matrix
  x$setimat(imat)                             #and stores it in the cache 
  imat                                        #returning imat
}

#testing
my_matrix<-matrix(c(2,3,4,5,6,7,3,4,6),3,3)
cacheSolve(makeCacheMatrix(my_matrix))

# In case you want to compute the inverse of a non-squared matrix, you could use the
## following function (taken from the MASS library, original name ginv). You simply need to run
## the function below and replace "solve" from  "inv_mat" on the previous code: 

inv_mat<- function (X, tol = sqrt(.Machine$double.eps)){
  if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
    stop("'X' must be a numeric or complex matrix")
  if (!is.matrix(X)) 
    X <- as.matrix(X)
  Xsvd <- svd(X)
  if (is.complex(X)) 
    Xsvd$u <- Conj(Xsvd$u)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive)) 
    Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
  else if (!any(Positive)) 
    array(0, dim(X)[2L:1L])
  else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
                                               t(Xsvd$u[, Positive, drop = FALSE]))
}
#It will be something like this: 

makeCacheMatrix <- function(x = matrix()) {         #creates a matrix
  imat <- NULL
  set <- function(y) {                        #stores the matrix in a different environment
    x <<- y
    imat <<- NULL                       #imat is the inverse of the x matrix, not yet calculated
  }
  get <- function() x                           #funciton that takes the matrix x as an argument          
  setimat <- function(inv_mat) imat <<- inv_mat     #sets the inverse of the x matrix with solve (only for squared matrices. Other dimensions see below)
  getimat <- function() imat                    #gets the inverse of the matrix x
  list(set = set, get = get,                    #list of all the above functions
       setimat = setimat,
       getimat = getimat)
}

cacheInv_mat <- function(x, ...) {                    #function that takes the x matrix created above as an argument
  imat <- x$getimat()                         #it gets the inverse of the matrix from the cache if it was already 
  if(!is.null(imat)) {                        #calculated(imat is not na (not missing))
    message("getting cached data")
    return(imat)                        #returns imat(the inverse of the matrix x)
  }
  data <- x$get()                             #if not calculated 
  imat <- inv_mat(data, ...)                    #gets the inverse of the matrix
  x$setimat(imat)                             #and stores it in the cache 
  imat                                        #returning imat
}
#testing
matrix <- matrix(1:6,2,3)
cacheInv_mat(makeCacheMatrix(matrix))
