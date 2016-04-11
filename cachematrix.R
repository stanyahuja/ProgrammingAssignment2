## makeCacheMatrix stores and retrieves a matrix and the matrix's inverse
## cacheSolve will compute the inverse of the matrix stored in makeCacheMatrix
## and cache it in the makeCacheMatrix object
## If the inverse has been calculated and cached previously 
## cacheSolve will retrieve it rather than recalculate it again.

## makeCacheMatrix: returns a list of four functions
## set: stores a matrix into the object
## get: retrieves the matrix from the object
## setinverse: calculated and caches the matrix inverse
## getinverse: retrieves the matrix inverse
## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
         # initialize by setting null for matrix inverse   
           # store a matrix in x set NULL for the inverse
                set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # retreive the stored matrix
        get <- function() x
        # store inverse of of x in inv
        setInverse <- function(inverse) inv <<- inverse
         # retrieve inverse of x (inv)
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
               ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
