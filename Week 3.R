## R Programming Week 3

## "Loop functions" in R
## Allow you to execute loop-like behavior in a compact form.
## These functions typically have the word "apply" in them and are
## particularly convenient when you need to execute a loop on the
## command line when using R interactively.

## Loop functions: lapply, apply, tapply, split, mapply

## lapply - Loop over a list and apply a function to each element 
## in a list.  always coerces to a list and returns a list
x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean) ## Find the mean of each element in the list x

x <- 1:4
## Generate four random uniform variables (runif) of length
## 1, 2, 3, and 4 between 0-1 (default)
lapply(x, runif) 

## Generate four random uniform variables (runif) of length
## 1, 2, 3, and 4 between 0-10
lapply(x, runif, min=0, max=10)  

## CReate a list with one 2x2 matrix and one 3x2 matrix
## and extract the first column from each matrix to a new list
x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x
lapply(x, function(elt) elt[,1])
## elt is an example of an anonymous function defined only for 
## the purpose of defining a variable to capture the elements 
## lapply captures as it loops through the list and then 
## specifying to extract the 1st column of vectors from the just
## the first column


## sapply - Same as lapply but simplify result
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20,1), d = rnorm(100,5))
x
## lapply(x, mean) returns the mean of each vector as a 
## separate element.
lapply(x, mean)
## But I want to return the mean as a list because it's easier
## to use the result that way
sapply(x, mean)

## apply - Apply a function over the margins of an array
## Useful to summarize matrices
## Create a 20x10 matrix of 200 random normal variables
x <- matrix(rnorm(200), 20, 10)
x
## The matrix has two dimensions:  rows and columns
## Rows are dimension 1 and columns are dimension 2
## Find the mean of each column by specifying margin value
## equal to the dimension to which you want to apply the function
apply(x, 2, mean)
## Find the sum of each of the rows
apply(x, 1, sum)
## CAUTION:  For calculating row and column sums and means
## use special functions rowSums, rowMeans, colSums and Colmeans
## These specials are optimized to work much faster and 
## efficiently than apply and differences in performance are 
## noticable with very large matrices

## Use apply for other functions, such as quantiles
## Find the 25th and 75th percentile values for each row
apply(x, 1, quantile, probs = c(0.25, 0.75))

## Suppose I have an array of 10 2x2 matrices
a <- array(rnorm(2*2*10), c(2,2,10))
## Find the average of the 2x2 matrices (i.e. the average
## of the corresponding row,col values of the matrices)
## to produce a 2x2 matrix.  In the following, c(1,2) 
## Will keep the rows and columns of each matrix, but collapse
## the third dimension.
apply(a, c(1,2), mean)
## Of course, I can also use rowMeans() to do the same thing
rowMeans(a, dims = 2)
## NOTE: 'dims' - integer: Which dimensions are regarded as 
## 'rows' or 'columns' to sum over. For row*, the sum or mean 
## is over dimensions dims+1, ...; for col* it is over 
## dimensions 1:dims.  In the above example dims = 2, so 
## the function collapses dim 2+1=3 or the third dimension
## which is the matrices of the array.

## mapply - Multivariate version of lapply
## Applies a function over a set of objects, not just one
## Create a list of 4 elements
list(rep(1, 4), rep(2,3), rep(3,2), rep(4,1))
## instead use mapply
mapply(rep, 1:4,4:1)

## Vectorizing a function


## tapply - Apply a function over subsets of a vector


## split - auxiliary function often used with lapply or sapply
## splits objects into pieces


## Lexical Scoping


## In this example we introduce the <<- operator which can be used 
## to assign a value to an object in an environment that is 
## different from the current environment. Below are two functions 
## that are used to create a special object that stores a numeric 
## vector and cache's its mean.


## Following function, makeVector, creates a special "vector", which ## is really a list containing a function to set or get the value or ## mean of a vector.

makeVector <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean,
          x = x,
          m = m)
}

## The following function calculates the mean of the special
## "vector" created with the above function.

cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}


