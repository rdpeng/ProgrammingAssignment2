


makeVector <- function(data_vector = numeric()) {
        
        stored_mean <- NULL       #Initialize with mean to NULL.
        
        set <- function(y) {   # The numeric arg pased into makeVector
                data_vector <<- y
                stored_mean <<- NULL
        }
        
        get <- function() {  # Create a function get in the makeVector
            return (data_vector)
        }
        
        setmean <- function(sent_replacement_mean){
            stored_mean <<- sent_replacement_mean
        }
        
        getmean <- function() {
            return(stored_mean)
        }
        
        
        list(set = set, get = get,  #list out the values of the functions in the makeVectors
             setmean = setmean,
             getmean = getmean)
}


cachemean <- function(madeVector, ...) {
        
        #goes to the "madeVector" and assings the "local_mean" value from that environment to this one
        local_mean <- madeVector$getmean() 
        
        if(!is.null(local_mean)) { 
                message("getting cached data")
                return(local_mean)
        }
        else {
            local_data <- madeVector$get()
            local_mean <- mean(local_data, ...)
        
            madeVector$setmean(local_mean)
            return(local_mean) 
        }
}
