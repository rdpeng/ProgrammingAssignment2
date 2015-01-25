makeCacheMatrix<- function(x = numeric()) {
        inversecache <- NULL #inverse NULL in function environment
        set <- function(y) {
                x <<- y
                inversecache <<-NULL
        } #set x in the cache + inverse empty in cache
        get <- function() x #the function returns x
        setinversecache <- function(inverse) inversecache <<- inverse #inverse set in the cash
        getinversecache <- function() inversecache # get inverse from the cash to put it
        list(set = set, get = get,
             setinversecache = setinversecache,
             getinversecache = getinversecache)
}
cacheSolve<- function(x, ...) { 
	  #requires a make cache matrix object, 
	  #and calculate the inverse of the matrix $get 
	  #either by using the cache $getinversecache or calculating it and setting the cache 
	  inverse <- x$getinversecache() #m initialised to value if inverse in the cache
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        } #if inverse not null it is returned and the function ends
        data <- x$get() #value of x vector in data
        inverse <- solve(data) #value of inverse of vector x computed in inverse
        x$setinversecache(inverse) # cache modified to set inverse cache to inverse
        inverse #result of the function
}




