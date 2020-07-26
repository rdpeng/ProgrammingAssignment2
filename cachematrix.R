## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(),...) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setMinv<-function(solve)
        i<<-solve
        getMinv<-function()i
        list(set=set,get=get,setMinv=setMinv,getMinv=getMinv)
}
        
        



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getMinv()
        if(!is.null(i)){
        message("minv cached data")
        return(i)
        }
        data<-x$get()
        i<-solve(data,... )
        x$setMinv(i)
        i
}







##Testing if it woeks

(A = matrix(rnorm(9),3,3))
         [,1]         [,2]       [,3]
[1,] 2.030663  0.885790699 -0.6578263
[2,] 1.642443 -0.009660492 -1.1423251
[3,] 1.423421  1.102589436 -1.0944199





> (B = makeCacheMatrix(A))
$set
function(y){
        x<<-y
        i<<-NULL
    }
<environment: 0x0000022c24abed58>

$get
function()x
<environment: 0x0000022c24abed58>

$setMinv
function(solve)
    m<<-solve
<environment: 0x0000022c24abed58>

$getMinv
function()i
<environment: 0x0000022c24abed58>






B$get() #the same as A
         [,1]         [,2]       [,3]
[1,] 2.030663  0.885790699 -0.6578263
[2,] 1.642443 -0.009660492 -1.1423251
[3,] 1.423421  1.102589436 -1.0944199





cacheSolve(B)#1st time
          [,1]       [,2]       [,3]
[1,] 0.8297349  0.1594774 -0.6651894
[2,] 0.1120470 -0.8401523  0.8095793
[3,] 1.1920507 -0.6390048 -0.9632602





cacheSolve(B)#2nd time
minv cached data
          [,1]       [,2]       [,3]
[1,] 0.8297349  0.1594774 -0.6651894
[2,] 0.1120470 -0.8401523  0.8095793
[3,] 1.1920507 -0.6390048 -0.9632602
