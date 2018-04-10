## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        InvMatrix <- NULL                                       ##set the InvMatrix object as null
        set <- function(y) {
                x <<- y                                         ##assign the value in diferent enviroments to x
                InvMatrix <<- NULL                              ##assign the value of null in multiple enviroments to InvMatrix
        }
        get <- function() x                                     
        setInvM <- function(solve) InvMatrix <<- solve          ##solve the Inverse Matrix and save the value in the multiple enviroments
        getInvM <- function() InvMatrix
        list(set = set, get = get,
             setInvM = setInvM,
             getInvM = getInvM)

}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        InvMatrix <- x$getInvM()                                ##Call the getInvM function and assign it´s value to InvMatrix
        if(!is.null(InvMatrix)) {                               ##Evaluate if the matrix has not been calculate the value should be NULL
                message("getting cached data")                  ##If the value in different from NULL print "Getting cache data"
                return(InvMatrix)                               ##Return the value stored in InvMatrix
        }
        data <- x$get()                                         ##If the value of InvMatrix is null the function will execute this code
	  InvMatrix <- solve(data)                              ##After get the matrix into data, with the function solved obtain the inverse matrix
        x$setInvM(InvMatrix)                                    ##Set the InvMAtrix value       
        InvMatrix                                               ##Return the invMatrix value
}

##Example of how this function works
> TestMatrix<-matrix(c(1,2,3,0,1,4,5,6,0),3,3,byrow=TRUE)       ##First we create the TestMatrix, for avoiding errors we search for a matrix with inverse for the example here is the link:http://www.purplemath.com/modules/mtrxinvr2.htm
> TestMatrix                                                    ##We print the matrix to make sure we have a 3 by 3 matrix
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    1    4
[3,]    5    6    0
> Test<-makeCacheMatrix(TestMatrix)                             ##we use our makeCacheMatrix function and store it in Test
> Test                                                          ##To make sure we don´t have error we print the value
$set
function (y) 
{
    x <<- y
    InvMatrix <<- NULL
}
<bytecode: 0x000000000acd0080>
<environment: 0x000000000b0c59c8>

$get
function () 
x
<bytecode: 0x000000000ad2c148>
<environment: 0x000000000b0c59c8>

$setInvM
function (solve) 
InvMatrix <<- solve
<bytecode: 0x000000000ad97b70>
<environment: 0x000000000b0c59c8>

$getInvM
function () 
InvMatrix
<bytecode: 0x000000000adf6ca0>
<environment: 0x000000000b0c59c8>

> cacheSolve(Test)                                              ##We solve the matrix, in this case the value of the InvMatrix variable was NULL
     [,1] [,2] [,3]                                             ##because this is the first time we solved after this the inverse matrix will be 
[1,]  -24   18    5                                             ##stored in InvMatrix in multiple enviroments because of the use of(<<-)
[2,]   20  -15   -4
[3,]   -5    4    1
> cacheSolve(Test)
getting cached data                                             ##In this second time we try to solve the same matrix Test the function retur
     [,1] [,2] [,3]                                             ## the message of "getting cached data" after that we print the value on the 
[1,]  -24   18    5                                             ##inverse matrix stored in InvMatrix
[2,]   20  -15   -4
[3,]   -5    4    1
