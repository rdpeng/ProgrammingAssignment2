### How to run

1. Source the cachematrix.R file - source('cachematrix.R').
2. Create the Matrix (shoud by square and Inversable).
3. Send the matrix to the makeCacheMatrix function, and save to an object.
4. Send the object (from step 3) to the cacheSolve function.
5. Now you can use object$get() and object$getinverse(), to get the cached matrix and inversed Matrix repetivly.

###  Example
```
cached <- makeCacheMatrix(matrix(c(4,7,12,8,2,13,9,24,3), nrow = 3, ncol = 3))
#Now lat's solve (get the Inverse Matrix)
cacheSolve(cached)
#Now lets print the solved Matrix from the cached object
print(cached$getinverse())

```

###  Test file
I made a text file - cachematrix_test.R
You can pen it in Rstudio, press the Source button, to test two cases (3x3, and 4x4 matrices).