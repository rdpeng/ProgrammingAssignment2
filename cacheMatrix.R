ProgrammingAssignment2/cachematrix.R
makeMatrix <- function(x=numeric(),z,c){
m <<- matrix()
m <- matrix(x,z,c)
setMatrix <<- m
inverseCheck <<- det(setMatrix)
n <<- inverseCheck
setInverse <<- solve(setMatrix)
setInverse <<- setInverse
list(setMatrix=setMatrix, setInverse=setInverse)}

cacheMatrix <- function(x=numeric(),z,c) {
setRows <<- z
setColumns <- c
setInverseold <- setInverse
makeMatrix(x,setRows,setColumns)
if( n == 0 ) { message("getting cached inverse data")
return(setInverseold)}
list(setInverse=setInverse, setInverseold=setInverseold)
}
