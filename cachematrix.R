##Matrix inversion is a costly operation ,makeCacheMatrix creates a cached matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL
	 set <- function(y){
	     	 x <<- y
		 i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set= set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix  and returns the result . 
## The result is retrived from cache if the matrix has not been changed

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if (!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i	   
}

## output :
#> x = rbind(c(1,3,9),c(3,2,1),c(3,1,4))
#> m = makeCacheMatrix(x)
#> p1 <- cacheSolve(m)
#> p1
#            [,1]        [,2]       [,3]
#[1,] -0.14893617  0.06382979  0.3191489
#[2,]  0.19148936  0.48936170 -0.5531915
#[3,]  0.06382979 -0.17021277  0.1489362
#> p2 <- cacheSolve(m)
#getting cached data
#> p2
#            [,1]        [,2]       [,3]
#[1,] -0.14893617  0.06382979  0.3191489
#[2,]  0.19148936  0.48936170 -0.5531915
#[3,]  0.06382979 -0.17021277  0.1489362
#> 
