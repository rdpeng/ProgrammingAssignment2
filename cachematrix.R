## To calculate inverse of a matrix using cache method

## Create a special matrix with cached memory allocation

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get <- function(){
        return(x)
    }
    setinverse <- function(inverse){
        m <<- inverse
    }
    getinverse <- function(){
        return(m)
    }
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## Calculate the inverse of a matrix or take it from cache if already present

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(is.null(m) == F){
        message("getting cached inverse of matrix")
        return(m)
    }
    data = x$get()
    m = solve(data)
    x$setinverse(m)
    return(m)
}
x = makeCacheMatrix(matrix(1:4,2,2))
x
cacheSolve(x)
