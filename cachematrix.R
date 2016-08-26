## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# inverse varialbe which contains the result of inverse matrix
# It is used as a global variable, it means everywhere can read/write this variable
#
inverse = NULL

# data variable which contains the original matrix
data = 0

# makeCacheMatrix
#   return set/get matrix x, set/get the result of inverse version matrix of x   
# parameter
#   x : matrix
# return
#   list : set / get / setInverse / getInverse function
#
#
makeCacheMatrix <- function(x = matrix()) {
    set = function(y) {
         data <<- y
         inverse <<- NULL
    }
    
    get = function() {
        data
    }
    
    setInverse = function(inverse) {
        inverse <<- inverse
    }
    
    getInverse = function() {
        inverse
    }
    
    # keep the original matrix
    set(x)
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
    cachedInverse = m$getInverse()
    
    # in case of using cached data
    if(!is.null(cachedInverse)) {
        print("Return Cached data")
        return(cachedInverse)
    }
    
    temp = m$get()
    inverse_temp = solve(temp)
    m$setInverse(inverse_temp)
    print("Return Uncached data")
    inverse_temp
}

sample1 = cbind(c(4, 2), c(7, 6))
m = makeCacheMatrix(sample1)

# the first trial - uncached data
cacheSolve(m)
# the secode trial - cached data
cacheSolve(m)
