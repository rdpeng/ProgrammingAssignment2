
#makecachematrix is a function to set, get, setInverse and getInverse
makecachematrix <-function(x = matrix()){
    inv <-NULL     #initializing inverse as NULL
    set <-function(y){
        x <<-y
        inv <<-NULL
    }
    get <- function() {x}    #function to get matrix x
    setInverse <-function(inverse) {inv <<- inverse}
    getInverse <-function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}

#This is used to get the cache data
cachesolve <- function(x, ...){
    inv <-x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)    #return inverse value
    }
    mat <-xiget()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
