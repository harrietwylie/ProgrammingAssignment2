## These functions are used to cache the inverse of the matrix, which saves computational time.

## The first function, makeCacheMatrix, is a function that returns a list of functions. It's purpose is to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(
             set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
        )

}


## This function calculates the inverse of the matrix created with makeCachematrix,
## reusing the cached result if already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
        
}
