INSTRUCTION:

Second programming assignment will require you to write an R function is able to cache potentially time-consuming computations. For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. In this Programming Assignment will take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.

Review Criteria
This assignment will be graded via peer assessment. During the evaluation phase, you must evaluate and grade the submissions of at least 4 of your classmates. If you do not complete at least 4 evaluations, your own assignment grade will be reduced by 20%.


<<-  In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores and calculate derivatives of x and y with the given functions and cache's its mean.

The first function, makeVector creates a special “vector”, which is really a list containing a function to:
1. set the derivative of x
2. get the derivative of x
3. set the derivative of y
4. get the derivative of y

Answer: 

makeCacheFunction < - function [Curvature_T] = given(V_x.V_y,2) {
%Input the value of x and y
%Input the value of time for t
%Define the radius of Curvature T    }


Curvature_T=[(V_x1^2+V_y1^2)^(1/2)/([V_x1*V_y2]-(V_y1*V_x2))];

{    
get <- Derivative2x = diff(V_x,2);
           V_x2 = subs(Derivative2x_1);
setDerivative <- Derivative2x = diff(V_x,2) <<- solveDerivative

get <- Derivative2y= diff(V_x,2);
           V_y2= subs(Derivative2y,1);
setDerivative <- Derivative2x = diff(V_x,2) <<- solveDerivative

list(set = set, get = get, 
 
             setDerivative = setDerivative, getDerivative = getDerivative)   }


<<- The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.


The following  calculates the derivative of "x and y” created with the above function. 

Answer:

cachemean <- function(x,y, ...) {
        d <- x$getDerivative()
        if(!is.null(d)) {
                message("getting cached data")
                return(d)
        }
        data <- x$get()
        d <- Derivative(data, ...)
        x$setDerivative(V_x,V_y)
        m
}


Assignment: Caching the Inverse of a Matrix
less 
Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

Write the following functions:

Write the following functions:

makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

For this assignment, assume that the matrix supplied is always invertible.

In order to complete this assignment, you must do the following:

Fork the GitHub repository containing the stub R files at https://github.com/rdpeng/ProgrammingAssignment2 to create a copy under your own account.

Clone your forked GitHub repository to your computer so that you can edit the files locally on your own machine.

Edit the R file contained in the git repository and place your solution in that file (please do not rename the file).

Commit your completed R file into YOUR git repository and push your git branch to the GitHub repository under your account.

Submit to Coursera the URL to your GitHub repository that contains the completed R code for the assignment.

In addition to submitting the URL for your GitHub repository, you will need to submit the 40 character SHA-1 hash (as string of numbers from 0-9 and letters from a-f) that identifies the repository commit that contains the version of the files you want to submit. You can do this in GitHub by doing the following

Going to your GitHub repository web page for this assignment

Click on the “?? commits” link where ?? is the number of commits you have in the repository. For example, if you made a total of 10 commits to this repository, the link should say “10 commits”.

You will see a list of commits that you have made to this repository. The most recent commit is at the very top. If this represents the version of the files you want to submit, then just click the “copy to clipboard” button on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the course web site when you submit your assignment. If you don't want to use the most recent commit, then go down and find the commit you want and copy the SHA-1 hash.
