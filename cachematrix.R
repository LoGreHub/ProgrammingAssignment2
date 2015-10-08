# Following functions provide a way of caching results of intense computional activity
# such as matrix inverse calculation, so that in case of more iterations of the mentioned
# computation data can be retrieved from cache instead of being recalculated.

# It is assumed the input matrix is always invertible (i.e. squared and non singular),
# so no invertibility check is not implemented in the code.


# makeCacheMatrix returns a special matrix object consisting in a list of 4 functions
# used to set/retrieve values of the matrix and its inverse

makeCacheMatrix <- function(X = matrix()) {

    invMatrix <- NULL

    set <- function(Y) {
        X <<- Y
        invMatrix <<- NULL
    }
    get <- function() X

    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


# cacheSolve expects output of the previous function as input and returns the inverse of the
# starting matrix

cacheSolve <- function(x, ...) {

    # checks if inverse has already been computed

    invMatrix <- x$getInverse()
    if(!is.null(invMatrix)) {
        message("getting cached data for matrix inverse")
        return(invMatrix)
    }

    # computes inverse and sets its value for future retrieval from cache

    orig_matrix <- x$get()
    invMatrix <- solve(orig_matrix, ...)
    x$setInverse(invMatrix)
    invMatrix

}

# Test sample

# first row creates new matrix m
# second row creates new list M
# third row calculates inverse for the first time
# fourth row recalculates inverse to show cache access

# m = matrix(rnorm(100),nrow=10)
# M = makeCacheMatrix(m)
# cacheSolve(M)
# cacheSolve(M)
 
