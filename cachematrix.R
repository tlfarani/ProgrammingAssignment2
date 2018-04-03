#### Assignment 2 - Week 3: Caching the Inverse of a Matrix ####

## Creates a cache matrix

makeCacheMatrix <- function(x = numeric(), nrow = numeric(), ncol = numeric()) {
                        x <- matrix(x, nrow, ncol)
                        inv <- NULL
                        set <- function(y, nrow, ncol) {
                                y <- matrix(y, nrow, ncol)
                                x <<- y
                                inv <<- NULL
                        }
                        get <- function() x
                        setinverse <- function(inverse) inv <<- inverse
                        getinverse <- function() inv
                        list(set = set, get = get,
                                setinverse = setinverse,
                                getinverse = getinverse)
}



## Gets the inverse of a matrix. Gets a cached value if the matrix the input matrix in not new.

cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)  ## Return a matrix that is the inverse of 'x'
                x$setinverse(inv)
                inv
}
       