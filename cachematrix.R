## The two functions combined together try to reduce the computations required in
#  calculating the inverse of a matrix. They initialize the inverse to NULL in the
#  beginning. When the first time the functions are called with a valid matrix, 
#  they store the inverse in cache. Post that whenever, the inverse function is 
#  called, it checks if the inverse already exists and the matrix is same. If yes 
#  for both, then the inverse is returned from cache otherwise, neam mean is 
#  calculated and stored in cache.

## The function below creates a list of functions which do the following
# 1) Get the value of matrix    2) Set the value of matrix 
# 3) Get the inverse of matrix  4) Set an initial value for the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<-NULL
    }
    get <- function() x
    setinv <- function(matrixinverse) inv <<- matrixinverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function below eventually returns inverse of a given matrix. However, it
#  checks the following conditions on the go
#  1) Checks if the inverse is already calculated and matrix is same. If yes, 
#     returns values from cache
#  2) If inverse is not present in the cache, then calculated the inverse, stores
#     it in the cache and returns the value

cacheSolve <- function(z, ...) {
    
    inv <- z$getinv()
    if (!is.null(inv) & z$get()==x) {
        message("Inverse for the matrix already exists. Getting inverse from cache")
        return(inv)
    }
    data <- z$get(x)
    inv <- solve(data)
    z$setmean(inv)
    inv
}
