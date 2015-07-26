## Caching the Inverse of a Matrix:
# The following include a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix:
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL # set the inverse matrix to NULL as a placeholder for a future value
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        } # define a function to set the matrix, x, to a new matrix, y, and resets the inverse matrix, xinv, to NULL
        get <- function() x # return the input matrix, x
        setinverse <- function(inverse) xinv <<- inverse # set the inverse matrix, xinv, to inverse
        getuinverse <- function() xinv # return the inverse matrix, xinv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # return the list containing all of the functions just defined
}


## cacheSolve:
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        xinv <- x$getinverse()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        } # If the inverse has already been calculated, then return the previously cached result.
        data <- x$get() # If not, get the matrix
        xinv <- solve(data, ...) # solve the inverse matrix
        x$setinverse(xinv) # set the inverse matrix in the cache 
        xinv # return a matrix that is the inverse of 'x'
}
