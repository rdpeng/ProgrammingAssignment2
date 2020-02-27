# This function creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x=matrix()){
        inv <- NULL
        set <- function(y) {
                x <<-y
                inv<<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

# This Function computes the inverse of the special "matrix" ...
# created by function makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed),
# then it should retrieve the inverse from the cache

cacheSolve <- function(x,...){
        # return a matrix that is inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
}


# Testing the above function
my_matrix <- makeCacheMatrix(matrix(1:4,2,2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)

my_matrix$set(matrix(c(2,2,1,5),2,2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
