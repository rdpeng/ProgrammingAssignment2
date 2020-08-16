## Program to show the Inverse of a Square matrix


makeCacheMatrix <- function(mtrx = matrix()){  ##creating a matrix that can cache the inverse, taking an argument 'mtrx' as matrix
        if(ncol(mtrx) == nrow(mtrx) && det(mtrx) != 0){   ##just making sure the matrix to be a square one.
        inverse_matrix = NULL   ##NULL representing null objects
        set <- function(a){   ##setting the value of the matrix using another function 'set'
                mtrx <<- a
                inverse_matrix <<- NULL  ##reseting it to null in cas of a new matrix
        }
        get <- function() mtrx   ##getting the value of the matrix 'mtrx' outside function 'set'
        setinverse <- function(inverse) inverse_matrix <<- inverse   ##setting the value of the inverse_matrix 
        getinverse <- function() inverse_matrix   ##getting the value of the inverse_matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        }
        else{
                message("Enter valid Square Matrix")
        }
}

cacheSolve <- function(mtrx,...){   ##defining a function named 'cacheSolve'
        inverse_matrix <- mtrx$getinverse()   ## this statement gets the inverse of the matrix 'mtrx' and assigns it to 'inverse_matrix'
        if(!is.null(inverse_matrix)){   ##using if function to check if there's already an inverse of the matrix 'mtrx' and returning or displaying the in the inver matrix named 'inverse_matrix' 
                message("Getting Cached Data")
                return(inverse_matrix)
        }
        matrix <- mtrx$get()
        inverse_matrix <- solve(matrix,...)   ##to get the inverse of the matrix 'mtrx',solve() is used 
        mtrx$setinverse(inverse_matrix)   ##setting the value in the cache using the 'setinverse()' function
        inverse_matrix   ##getting the inverse matrix 'inverse_matrix'
}
