## These functions are for the storing and recalling of an inverse matrix from
## cache where the inverse is not present already. Its purpose is to reduce the
## computation by using the memory to store previously calculated functions.



makeCacheMatrix <- function(x = matrix()) {

##------------------------------Description-------------------------------------
##    This function performs the caching functions of the input and stores the 
#     inverse matrix
      
##ARGS 
#     'x' as matrix
#     'y' internal argument to pass the matrix into cache
#     'm' return matrix
#RETURNS
#      
#     A list of $parameters associated with the matrix object that is created
#     $setMatrix
#     $getMatrix
#     $setInvMatrix      
#     $getInvMatrix 
      
##SKIP THIS SECTION FOR ASSESSMENT OF ThE TASK - THIS IS JUST PRACTICing AND 
##SHARING SOME THINGS I FOUND HELPFUL IN R STUDIO, I LEARNT THAT CREATING
##COMMENTS FOLLOWED BY FOUR CONSECUTIVE DASHES--- CREATES A USEFUL MARKER IN THE
##SCRIPT WINDOW.##I USE THIS TO SPLIT UP THE FUNCTION INTO SECTIONS, 
##INTRODUCTIONS, INPUTCHECKS, MAIN BODY & OUTPUT
##HOPE YOU FIND THIS USEFUL
##----------------------------0-Input Checks-----------------------------------
      #insert code here to test the input arguments before proceding
      
##----------------------------1-Main Function-----------------------------------     
      #1-Create an empty variable called 'm' when initialised----
      m <- NULL
      #2 Set the matrix, and clear the cached inverse (since its no longer valid)
      setMatrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      #3 Return the matrix
      getMatrix <- function() x
      
      #4 Set/get the inverse matrix
      setInvMatrix <- function(inverse) m <<- inverse
      getInvMatrix <- function() m
      
##----------------------------9-Return -----------------------------------------
      #4 return all of the above as a list
      list(setMatrix = setMatrix, 
           getMatrix = getMatrix, 
           setInvMatrix = setInvMatrix, 
           getInvMatrix = getInvMatrix)
}


cacheSolve <- function(x, ...) {
##------------------------------Description-------------------------------------
##    This function perfroms the inverse function itself or returns a cached 
# value back to makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.
      
##ARGS 
      #'x' as matrix
#RETURNS
      #'m' as inverse result
      
##SKIP THIS SECTION FOR ASSESSMENT OF ThE TASK - THIS IS JUST PRACTICing AND 
##SHARING SOME THINGS I FOUND HELPFUL IN R STUDIO, I LEARNT THAT CREATING
##COMMENTS FOLLOWED BY FOUR CONSECUTIVE DASHES--- CREATES A USEFUL MARKER IN THE
##SCRIPT WINDOW.##I USE THIS TO SPLIT UP THE FUNCTION INTO SECTIONS, 
##INTRODUCTIONS, INPUTCHECKS, MAIN BODY & OUTPUT
##HOPE YOU FIND THIS USEFUL
##----------------------------0-Input Checks-----------------------------------
      #insert code here testing the input arguments before proceding
##----------------------------1-Main Function-----------------------------------     

      
##-1--Get the cached input----
      m <- x$getInvMatrix()
      
      ##-1A--if the inverse has been retrieved return it and skip the rest of the script----
      if(!is.null(m)) {
            message("Getting cached data...")
            #-1A4-return the result----
            return(m)
      }
      
      ##-1B--if the inverse hasn't been retrieved, calculate it----
      if(is.null(m)) {
            message("Looks like I'll be calculating this now then...")
            data <- x$get()
            message("please wait ...")
            #1B2 apply the solve function to the matrix
            m <- solve(data, ...)
            #1B3 store the result in cache
            x$setInvMatrix(m)
            #1B4 return the result----
            m
      }
}
