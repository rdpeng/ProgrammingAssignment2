## This Function geneartes a special vector  
## takes the input matrix as its argument

MakeCacheMatrix <- function(x = matrix()) {

        i<-NULL
        set<-function(y) {
              x<<-y
              i<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse <- function() i
        list(set=set, get = get,
             setinverse = setinverse,
             getinverse = getinverse )
}



## This function takes the special vector as the input and calculates the inverse of an input (invertible ) matrix

CacheSolve <- function(x, ...) {
                 
        # get the inverse stored for input matrix (x) in m
        m <- x$getinverse()

        # Check if the inverse was previously calculated and stored.

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        # if it is not stored in Cache then calculate the inverse

        data <- x$get()
        m <- solve(data, ...)

        # store input matrix and its inverse (m) in x
        x$setinverse(m)

        # ouput the inverse matrix
        m
}
