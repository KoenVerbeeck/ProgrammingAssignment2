# "Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly."
#
# This function will create a cached instance of a matix and of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    #
    # Arguments:
    # x = matrix. Default is an empty matrix.
    #
    # Return arguments:
    # A list with all the get and set functions of the special matrix.
    #
    
    inverse <- NULL # Initialize inverse to NULL
    
    # The set function will reset the matrix inside the wrapper. The inverse will be reset to NULL.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x # return the matrix
    setinverse <- function(inv) inverse <<- inv # set the inverse
    getinverse <- function() inverse # get the inverse
    
    # Return a named list with all of the get and set functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function computes the inverse of a special "matrix" returned by the makeCacheMatrix function.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    #
    # Arguments:
    # x = a special matrix, created with the makeCacheMatrix function.
    #
    # Return arguments:
    # The (cached) inverse of the matrix x.
    #
    
    inverse <- x$getinverse() # get the cached inverse of x. If it doesn't exist, the result will be NULL.
    
    # Check if the inverse already exists. If it does, show a message and return the inverse (finishing the function).
    if(!is.null(inverse)) { 
        message("getting cached data")
        return inverse
    }
    
    # The inverse does not exist, so it has to be calculated and stored inside the wrapped version of the matrix x.
    data <- x$get()
    inverse <- solve(data, ...) # solve calculates the inverse of a matrix
    x$setinverse(inverse)

    # Return the inverse.
    inverse
}