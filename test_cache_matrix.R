test_cache_matrix = function(){
    
# Call with one set of data. Then call with the same data.
# ========================================================
    set.seed(1)
    r = rnorm(16)
    matrix_01  = matrix(r, nrow=4, ncol=4)

    temp_01 = makeCacheMatrix(matrix_01)

    start.time = Sys.time()
    cacheSolve(temp_01)
    dur = Sys.time() - start.time
    print(dur)
    
    start.time = Sys.time()
    cacheSolve(temp_01)
    dur = Sys.time() - start.time
    print(dur)
    
# ====================================================================
# Call with different set of data. Then call with the same data again.
# ====================================================================
    set.seed(2)
    r = rnorm(4000000);
    matrix_02  = matrix(r, nrow=2000, ncol=2000);
    
    temp_02 = makeCacheMatrix(matrix_02);

    start.time = Sys.time();
    cacheSolve(temp_02);
    dur = Sys.time() - start.time
    print(dur);
    
    start.time = Sys.time()
    cacheSolve(temp_02);
    dur = Sys.time() - start.time
    print(dur)

# =============================================================
# Now recall with both sets of data again.
# ==============================================================
    cacheSolve(temp_01);
    cacheSolve(temp_02);
}