##   makeMatrix will create a list containing a function
##   to do the following:
##      1. set construct of the matrix
##      2. get the construct of the matrix
##      3. set the inverse value of the matrix
##      4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set = function(y) {    ## assign objects to value in different environemnt
                x <<- y
                i <<- NULL  
        }
        get <- function() x
        seti <- function(inv) i <<- inv
        geti <- function() i
        list(set=set, get=get,
             seti = seti,
             geti = geti)
}


##  cacheSolve calculates the inverse of the matrix above
##  first checks to see if the inverse has already been calculated
##  otherwise calculate inverse of the matrix
##  via the setmean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        i <- x$geti()
        
        ## check to see if inverse has already been calculated
        ## if exists let user know it is and display the inverse 
        ## and end the function,, skipping the calculation process
        
        if(!isnull(i)){
                message("getting cahched data")
                return(i)
        }
        
        ##calculating the inverse
        data <- x$get()
        i <- solve(data,...)  ##using ?solve found this to solve for simple vector or matrix
        x$seti(i)
        return(i)
}
