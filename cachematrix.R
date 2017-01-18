## Put comments here that give an overall description of what your
## functions do
# This two functions are used to access the inverse of a matrix in
# a computation-saving way.

## Write a short comment describing this function
# makeCacheMatrix use to store the original matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <<- NULL
    set_mtx <- function(y) {
        x <<- y
        m <<- NULL
    } 
    get_mtx <- function() x
    set_inv <- function(inv) m<<-inv
    get_inv <- function() m
    list(set_mtx = set_mtx,
         get_mtx = get_mtx,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Write a short comment describing this function
# The function get matrix generated from makeCacheMatrix,
# if the inverse of the matrix has been computed, then 
# cacheSolve get it from the cache, otherwise compute
# inverse and save it to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inv()
    if(!is.null(m)) {
        message("Getting the cached inverse")
        return(m)
    }
    m <- solve(x$get_mtx())
    x$set_inv(m)
    message("Inverse gets computed")
    m
}
