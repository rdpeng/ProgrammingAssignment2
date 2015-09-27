## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        getMatrix <- function() m
        setMatrix <- function(matrix) m <<- matrix()
        get <- function() {
                x
                print('--')
                print(x)
        }
        set <- function(y){
                x <<- y
                print(x)
                m <<- matrix()

        }
        list(set=set,setMatrix=setMatrix,get=get,getMatrix=setMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x =matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
       res <- x$getMatrix()
       if(is.null(res)){
           inverse <- solve(x)
           x$setMatrix(inverse)
       }
       else{
          message('Caching the result')
          m <- x$getMatrix()
          return(m)
       }
     m
}
