## The below functions are used to find the inverse of a matrix. 

# This will create an object to save inversed matrix - the funtion getmtrx() will 
# get the original matrix we need to inverse - function stmtrx() will assign 
# computed inverse to MtrxInvrsd - function gtmtrx() will get the cached inversed matrix

makeCacheMatrix <- function(x=matrix()) {
    MtrxInvrsd <- NULL
    getmtrx <- function() x
    stmtrx <- function(matrixI) MtrxInvrsd <<- matrixI
    gtmtrx <- function() MtrxInvrsd   
    list(getmtrx=getmtrx, stmtrx=stmtrx, gtmtrx=gtmtrx)
}




#This Function will get the matrix and inverse it and return the inversed matrix

cacheSolve <- function(x) {
    MtrxInvrsd <- x$gtmtrx()
    if(!is.null(MtrxInvrsd)){    
        return(MtrxInvrsd)
        }else {        
        mtrxDTA <- x$getmtrx() 
        MtrxInvrsd <- solve(mtrxDTA) 
        x$stmtrx(MtrxInvrsd) 
        return(MtrxInvrsd)
    }
}
