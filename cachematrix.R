##this function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     n <- NULL
     set <- function(y){
         x <<- y
         n <<- NULL
       }
   get <- function() x
   setmean <- function(mean) n <<- mean
   getmean <- function() n
   list(set = set, 
        get = get,
        setmean = setmean,
        getmean = getmean)
}

##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then it should retrieve the inverse from the cache

## Return a matrix that is the inverse of 'x'
 cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    
    n <- x$getmean ()
     if(!is.null(n)){
             message("gettig cached data")
         return(n)
         
         }
     data <- x$get()
     n <- solve(data, ... )
     x$setmean(n)
     n
 }
 
 ##Testing my Functions
 > sample <-makeCacheMatrix(matrix(1:4, 2, 2))
>  sample$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
>  sample$getmean ()
NULL
>  cacheSolve(sample)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
>  
>  sample$getmean()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
>  sample$set(matrix(c(2, 2, 1, 4), 2, 2))
>  sample$get()
     [,1] [,2]
[1,]    2    1
[2,]    2    4
>  sample$getmean()
NULL
>  cacheSolve(sample)
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
>  sample$getmean()
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> 
