## Put comments here that give an overall description of what your
## functions do


## NOTE01: Footnotes in the short comments I write look like this:  [^1], [^2], 
##        etc...and can be vieId at the bottom of the script.

## NOTE02:  Everything labeled "Checks." in this document uses the content 
##        provided from Berger (2017), "Simple test matricies for the
##        lexical scoping programming assignment" and is intended for use
##        according to the JHSPH Student Handbook on Referencing.  I have
##        attempted to replicate the Chicago Style as closely as possible.
##        When needing to show idalics, I code in LaTeX markdown style, for 
##        example \emph{R Programming for Data Science} is the class text-
##        book and the \emph{} is intended to show italics.

##    Write a short comment describing this function...
##  The two functions in this script, `makeCacheMatrix()` and `cacheSolve()`
##  follow the same methodology as the `makeVector()` and `cachemean()` 
##  functions described by Greski (2020)[^1].  The `makeVector()` function
##  has one formal variable[^2], `x` that Greski (2020) defaults to 
##  a `numeric()` input.  Likewise, for `makeCacheMatrix()` the default
##  input must be of a `matrix()` value.  The value of the lexical scoping
##  rules for these two functions comes in their ability to set and get
##  
##  creates a list, as outlined in the "Demystifying makeVector()"[^3] article.  
##    The matrix is really a list that, when assigned as an object, 
##  contains four functions in a list:  set(), get(), 
##  setmean(), and getmean().  These are "setters and getters"[^4], which 
##  I use in the `makeCacheMatrix()` function below.  
##    Below this code I test it out with matricies provided by Dr. Berger's 
##  example matricies.[^5]
##    According to Peng (2019), "With lexical scoping the value 
##  of y in the function g is looked up in the environment in which 
##  the function was defined, in this case the global environment, 
##  so the value of y is 10. With dynamic scoping, the value of y is 
##  looked up in the environment from which the function was called 
##  (sometimes referred to as the calling environment). In R the 
##  calling environment is known as the parent frame. In this case, 
##  the value of y would be 2."[^6]  This phenomenon is especially
##  important in the `makeCacheMatrix()` and `cacheSolve()` functions
##  because the functions nested in the list are specific to the
##  environments from which they're called.  Put differently, if they
##  Ire global objects they would be changed each time any function
##  called them and thus a new computation (thus defeating the purpose)
##  would have to be completed each time.

##    The `makeCacheMatrix()` function is a list that sets 
##  and gets functions that do these four things: 
##  1) set the value of the matrix, next they; 2) get the value 
##  of the matrix, then they; 3) set the value of the inverse, 
##  and finally; the function; 4) gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the value of the matrix inverse to NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    ## change the value of the matrix inverse when the inverse is changed
    m <<- NULL
  }
  ## get the value matrix inverse
  get <- function() x
  ## use solve() to calc the inverse non-singula matrix
  setMatInv <- function(MatInv) m <<- solve
  ## get the inverse
  getMatInv <- function() m
  ## make the values calculated and cached above available in a list as lexically scoped objects
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)
}

## Example 1.
## From Berger (2017).
# Matrices for testing the R functions 
# makeCacheMatrix and cacheSolve
# in the Coursera R Programming Course
#
# First, 
# If you haven't read Leonard Greski's invaluable
# [TIPS] Demystifying makeVector()  Post
# be sure to do so.
#
# A simple matrix m1 with a simple matrix inverse n1
# Define
   m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
   m1
   I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
   I2
   n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
   n1
##  Checks.
   m1 %*% n1
   n1 %*% m1
   solve(m1)
   solve(n1)
##    These code checks for lines 73 - 82 above this comment should have
##  the same results as Berger (2017)^4, which is accurate.

##    Write a short comment describing this function
##    The next function `cacheSolve()` computes the inverse of the matrix
##  created by `makeCacheMatrix()`.  Like the `cachemean()` function,
##  the `cacheSolve()` function makes individual calls from the 
##  `makeCacheMatrix()` function by referencing using the $ operator.
##  Greski (2020) specifies the behavior at the end of 
##  "Demystifying makeVector()"
##      the lexical scoping assignment in R Programming takes advantage of 
##      lexical scoping and the fact that functions that return objects of 
##      type list() also allow access to any other objects defined in the 
##      environment of the original function. In the specific instance of 
##      makeVector() this means that subsequent code can access the values 
##      of x or m through the use of getters and setters. This is how 
##      cachemean() is able to calculate and store the mean for the input 
##      argument if it is of type makeVector(). Because list elements in 
##      makeVector() are defined with names, I can access these functions 
##      with the $ form of the extract operator.[^7]
##    Lexical scoping makes the cache possible when using the $ operator
##  in the `cacheSolve()` function below in 
##  m <- x$getMatInv() and in 
##  data <- x$get(), which references the get <- function() x above.
##  Put differently, anytime the `cacheSolve()` function uses the $ operator
##  it is using lexical scoping to call the `makeCacheMatrix()` object
##  tied to the operator.  If R used dynamic scoping, like in the C language
##  then those esoteric objects would be open to the global environment.
##  Finally, `cacheSolve()` retrieves the inverse from the cached 'matrix'
##  in the `makeCacheMatrix()` list.

#####   SECTION II.   #####
## This function calls the get and set objects from the `makeCacheMatrix()` function.
cacheSolve <- function(makeCacheMatrix.object, ...) {
  ## Return a matrix that is the inverse of 'makeCacheMatrix.object'
  m.local <- makeCacheMatrix.object$getMatInv()
  ## if the inverse exists, the call it
  if(!is.null(m.local)) {
    message("Getting cached data")
    return(m.local)
  }
  ## if the inverse does not exist, then calculate it and print it.
  data <- makeCacheMatrix.object$get()
  m.local.calculated <- solve(data, ...)
  makeCacheMatrix.object$setMatInv(m.local.calculated)
  m.local.calculated
}

##  Continuing from our check examples above, I create the object

myMatrix_object <- makeCacheMatrix(m1) ## and then run the object

##  through the `cacheSolve()` function.

cacheSolve(myMatrix_object)

## calling `cacheSolve()` a second time should retriev, and not
## recalculate, the matrix from n1.

cacheSolve(myMatrix_object)  ## which is successfully does by out-

##  putting the following:
##    > cacheSolve(myMatrix_object)
##    Getting cached data
##    function(a, b, ...)
##    UseMethod("solve")
##    <bytecode:  0x7ff62a0f78f0>
##    <environment: namespace:base>

##    The `setMatInv()` function and calculate a new matrix, I use n2
##  as an example:

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)

##    If I pass n1, calculated above, into `cacheSolve()` I get an error
##  message: "Error in n1$getMatInv : $ operator is invalid for atomic vectors"
##  that I expect.  Like the `cachemean()` function, the `cacheSolve()`
##  "REQUIRES an input argument of type `makeCacheMatrix()`." [^8]  Since 
##  primitive vectors are not lists and it does not contain a functions
##  called, `$getMatInv()`.  The `cacheSolve()` function completes the
##  `makeCacheMatrix()` function because the `cacheSolve()` function is
##  designed to require a population and/or retrieval of the matrix from
##  an object of type `makeCacheMatrix()`.




## Footnotes:
## 1.   Greski, Len. 2020. "Demystifying makeVector()." \emph{Coursera R Programming Course}.
##        https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md 
##        Accessed on 20 APR 2020.
## 2.   Peng, Roger D. 2019. "Scoping Rules of R." \emph{Leanpub}: 82.
## 3.   Greski. 2020. https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md 
##        Accessed on 20 APR 2020.
## 4.   Ibid.
## 5.   Berger, Alan E. 2017. "Simple test matrices for the lexical scoping 
##        programming assignment." \emph{Coursera R Programming Course}. 
##        https://www.coursera.org/learn/r-programming/discussions/Ieks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
##        Accessed on 20 APR 2020.
## 6.   Peng. 2019: 85.
## 7.   Greski. 2020: "Conclusion: what makes cachemean() work?"  
##        https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md 
##        Accessed on 20 APR 2020.
## 8.   Greski. 2020. "makeVector() step by step, step 3." 
##        https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md 
##        Accessed on 20 APR 2020.