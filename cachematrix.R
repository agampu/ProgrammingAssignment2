## This file contains two functions that implement a matrix object that
## caches its inverse.  Usage:
## CacheM <- makeCacheMatrix(m)  :Convert m into an inverse-cached matrix
## cacheSolve(CacheM)            :compute(or get cached) inverse
## CacheM.set(new_matrix_m)      :Change the matrix
## CacheM.setinverse(inv)        :Explicitly replace the inverse
## CacheM.getinverse()           :Returns the inverse, maybe NULL
## Calls to set() trigger a recompute of the inverse on the next CacheSolve.


## Takes a regular matrix and converts into an inverse-cached one.
makeCacheMatrix <- function(x = matrix()) {
    ## I think its beyond the scope of the assignment, but ideally
    ## we should test here if the matrix is invertible. I am only including
    ## a quick check that it should at least be square.
    if (dim(x)[1] != dim(x)[2]) {
      stop('Matrix not square')
    }
    inv <- NULL
    set <- function(new_matrix) {
        if (identical(x, new_matrix)) {
          return
        }
        ## so we assign this to the variable that stores our matrix.
        ## without the <<- operator, it would just create a local
        ## variable x and inv and assign to them which we don't want.
        x <<- new_matrix
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inv) inv <<- new_inv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## If there is a valid cached inverse, return it. If not, compute a new
## inverse value based on the matrix, return it, and cache it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting Cached Inverse")
        return(inv)
    }
    ## Current inverse is NULL. Recompute; store it and return it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
