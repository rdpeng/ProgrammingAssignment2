## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Call the function makeCacheMatrix with a matrix as an argument. The matrix will be stored in x.
##Initially the inverse which is denoted by inv is set to NULL.
##The function getMat when called, returns the original matrix and the function getinv returns the inverse of the matrix from the cache.
#The function setinv is used to store the inverse of the matrix as cache.
#Finally, the function makeCacheMatrix returns a list containing 3 functions described above.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        getMat<-function() x
        setinv<-function(inverse) inv<<-inverse
        getinv<-function() inv
        list(getMat=getMat,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function
##This function cachesolve first checks if the inverse of the matrix exists in the cache or not. If yes, it fetches it from there and returns the inverse.
##If not, it calculates the inverse, saves the inverse into cache using setinv() and returns the inverse of the matrix.
##x$get<Mat is used to extract the original matrix for inversing it and solve() inverses the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invers<-x$getinv()
        if(!is.null(invers){
                message("Inverse exists in cache. Fetching...")
                return(invers)
        }
        matri<-x$getMat()
        invers<-solve(matri)
        x$setinv(invers)
        invers
        
}
