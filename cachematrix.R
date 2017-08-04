## This function takes a matrix and returns
## an object, that allows for lazy evaluation
## and caching the inverse of that matrix.
## 
## The actual evaluation and caching is done
## by cacheSolve (see below).
##
## Args:
##   x: Matrix from which the inverse should be
##      cached
##
## Returns:
##   The object that allows for caching

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    ## This function sets the matrix and
    ## resets any cached value for the inverse
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }

    get <- function()
    {
        x
    }

    setinv <- function(i)
    {
        inv <<- i
    }

    getinv <- function()
    {
        inv
    }

    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function returns the inverse of a matrix,
## using the output of makeCacheMatrix (see above).
## 
## It only evaluates the inverse once and caches
## the result.
##
## Args:
##   x:    The output of makeCacheMatrix (above)
##   ... : Arguments to be passed to UseMethod("solve")
##
## Returns:
##   The inverse of a matrix, contained in x.

cacheSolve <- function(x, ...) {

    inv <- x$getinv()

    if (is.null(inv))
    {
        inv <- solve(x$get(), ...)
        x$setinv(inv)
    } else {
        message('Getting cached value')
    }

    inv
}
