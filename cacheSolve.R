cacheSolve <- function(x, ...) {
        m <- x$getinverse()
                #Calls the Inverse of the Matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m) #Return Inverse and stop
        }
                #Messages "getting cached data", 
                #if Inverse is not NULL it procedes, 
                #otherwise it shows NULL
        data <- x$get()
                #Calls Input Matrix from "makeCacheMatrix"
        m <- solve(data)
                #Creates Inverse of it
        x$setinverse(m)
                #Substitutes the Inverse in the List from
                #"makeCacheMatrix" and saves it in the Cache
        m
                #Shows the created Inverse
}

#The following function calculates the inverse of the 
#list created with "makeCacheMatrix". It first checks,
#if the Inverse is already calculated and if so, returns
#it from the Cache. If it does not exists, it calculates
#the Inverse and saves it in the Cache.

#Works under the assumption that the Input Matrix is 
#a square invertible matrix.