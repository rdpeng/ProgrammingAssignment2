# Execution example


### Generating a matrix
    > x <- matrix(c(1, 1, 2, 2), nrow = 2, ncol = 2)
    > x
    [,1] [,2]
    [1,]    1    2
    [2,]    1    2

### Calculating the inverse of x matrix
    > ginv(x)
         [,1] [,2]
    [1,]  0.1  0.1
    [2,]  0.2  0.2

### Invoking makeCacheMatrix function and verifying the special object returned
    > m = makeCacheMatrix(x)
    > m
    $set
    function (y) 
    {
        x <<- y
        inv <<- NULL
    }
    <environment: 0x00000000157050a0>
    
        $get
    function () 
    {
        x
    }
    <environment: 0x00000000157050a0>
    
        $setInv
    function (inverse) 
    {
        inv <<- inverse
    }
    <environment: 0x00000000157050a0>
    
        $getInv
    function () 
    {
        inv
    }
    <environment: 0x00000000157050a0>

### Getting the original matrix with get function
    > m$get()
         [,1] [,2]
    [1,]    1    2
    [2,]    1    2

### The inverse matrix is NULL yet
    > m$getinv
    NULL

### Invoking cacheSolve function
    > cacheSolve(m)
         [,1] [,2]
    [1,]  0.1  0.1
    [2,]  0.2  0.2

### Now, the getInv function returns the inverse matrix
    > m$getInv()
         [,1] [,2]
    [1,]  0.1  0.1
    [2,]  0.2  0.2

### Next call to cacheSolve function returns inverse matrix from cache
    > cacheSolve(m)
    getting cached data
         [,1] [,2]
    [1,]  0.1  0.1
    [2,]  0.2  0.2

### Modify original matrix in order to verify that the inverse matrix is destroyed
    > m$set(matrix(c(1, 1, 2, 2), nrow = 2, ncol = 2))
    > m$getInv()
    NULL
