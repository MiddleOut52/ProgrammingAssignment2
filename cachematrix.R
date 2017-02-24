## These functions are going to calculate and store the inverse of a matrix

## This function creates a matrix that can store the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) m<<- solveMatrix
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
                
}


## This function computes the inverse matrix returned by the function above.  If the invers matrix has 
## already been calculated, that value is returned.

cacheSolve <- function(x, ...) {
        m <-  x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
