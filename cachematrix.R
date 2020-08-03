##My transformation: In the sample code I have replaced m with s. 
##As input I have placed a matrix (x). I have assigned s = NULL.
##In the rest of the code I have changed the mean function for the solve function.
##In makeCacheMatrix, for a given matrix (generated in some way) a list 
##is established that generates and contains the matrix, and generates and 
##contains its inverse (in our case, for the test example, the matrix was 
##generated with standardized normal random numbers, is square of order three)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
  x <<- y
  s <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) s <<- inverse
  getInverse <- function() s 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}



##The cacheSolve code returns the inverse of the generated matrix, if it has 
##not been calculated before. If the inverse value has already been calculated,
##then it returns that cached value.


cacheSolve <- function(x, ...) {
  s <- x$getInverse()
  if(!is.null(s)){
  message("getting cached data")
  return(s)
  }
  mat <- x$get()
  s <- solve(mat,...)
  x$setInverse(s)
  s
}

## My example
> A <- makeCacheMatrix()
> A
$set
function(y){
  x <<- y
  s <<- NULL
  }
<bytecode: 0x000000001549ab70>
<environment: 0x000000001765f918>

$get
function()x
<bytecode: 0x0000000014b8bf68>
<environment: 0x000000001765f918>

$setInverse
function(inverse) s <<- inverse
<bytecode: 0x00000000159ebf60>
<environment: 0x000000001765f918>

$getInverse
function() s
<bytecode: 0x00000000152ee920>
<environment: 0x000000001765f918>

> A$set(matrix(rnorm(81, mean = 0, sd = 1), 9, 9))
> A$get()
            [,1]       [,2]       [,3]       [,4]       [,5]       [,6]
 [1,]  1.0571177  0.4495906  1.2828903  0.9661272  1.6900789 -1.2916284
 [2,] -0.3660224  0.5750962  0.7718974  1.0517861 -0.8184293  0.3257654
 [3,]  0.9614140  1.8115396  0.2451662  0.6336019 -0.1695890  0.4315164
 [4,] -0.9164453  0.2779890 -0.9619781  0.7907331 -0.1695980 -0.5846152
 [5,]  1.6265208 -0.2958575 -0.7360079 -0.1185900 -1.9073718  1.0754754
 [6,]  0.4377567  1.3396013  1.0246344  1.9760054 -0.1445998  1.3734946
 [7,] -0.8394360  1.1765474  0.5525843 -0.1408976  0.3932049  0.5128462
 [8,]  0.8539253 -0.7846112 -0.6963668  1.0942520 -0.6977235 -0.3760106
 [9,]  0.7059092 -0.8136528  0.3919482 -0.9606134  0.2999417  1.5002321
            [,7]        [,8]        [,9]
 [1,] -0.1345626  0.16779758  0.31362619
 [2,]  0.5434543  0.74341098 -2.60060228
 [3,]  0.7043629 -0.84522620  0.61832189
 [4,] -2.0519386  0.14942485 -0.10984575
 [5,] -1.5632132  1.22927523  2.15121875
 [6,]  0.9749795  0.54070502  1.11937893
 [7,]  2.4889987  0.81475095  0.01886957
 [8,] -0.3238904  0.53400556 -1.55348218
 [9,]  0.2277899 -0.09780282 -1.69874344
> A$setInverse(solve(A$get()))
> A$getInverse()
             [,1]        [,2]         [,3]        [,4]        [,5]        [,6]
 [1,]  0.19377219 -0.08011003  0.254489833 -0.09138125  0.15009719 -0.14411616
 [2,]  0.13344685  0.13969097  0.421968564  0.33140019  0.13822519 -0.25337422
 [3,]  0.13779066  0.48317944 -0.220060678 -0.39372427  0.04335411  0.08552131
 [4,] -0.07400954 -0.11225212 -0.102262745  0.02390125 -0.19013849  0.40006155
 [5,]  0.22967313 -0.39528575 -0.024393532  0.38147884 -0.11841765  0.07679964
 [6,] -0.15076446 -0.15593286 -0.024122471  0.19828028 -0.02486139  0.29365252
 [7,] -0.11797507 -0.12638354  0.006592224 -0.27349040 -0.10613960  0.03835002
 [8,]  0.24589625  0.04716586 -0.206397232  0.22754475  0.28887525 -0.11370679
 [9,] -0.03232300 -0.15240570 -0.102148848 -0.10837182  0.03996085  0.13956205
             [,7]        [,8]        [,9]
 [1,]  0.04851111  0.23124930  0.14113129
 [2,]  0.28917302 -0.14772070  0.08933083
 [3,] -0.41980480 -0.53214900 -0.17566208
 [4,] -0.16189481  0.27094484 -0.10732453
 [5,]  0.29877580  0.14271210  0.38745599
 [6,]  0.02537452 -0.03737794  0.38576213
 [7,]  0.15761155  0.24022573 -0.13528970
 [8,]  0.47610673  0.08220057  0.10436231
 [9,] -0.05477264 -0.08947933 -0.16770576

