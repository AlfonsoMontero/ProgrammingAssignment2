## Using R scoping rules this functions computes the inverse of a matrix and
## cache the result.

## This function makes a "special" matrix with cache for its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function (m){
                x <<- m
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(m) inverseMatrix <<- m
        getInverse <- function() inverseMatrix
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function compute the inverse of a matrix 
##or return cached previuosly computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getInverse()
        if (!is.null(im)) return(im)
        m <- x$get()
        im <- solve(m,...)
        x$setInverse(im)
        im
}
