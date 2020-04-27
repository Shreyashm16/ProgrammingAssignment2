## create a matrix using the function makeCacheMatrix and then calculate and store the inverse 
## using cacheSolve(my_matrix). You can display the matrix using 'name of matrix'$getInverse()

## Write a short comment describing this function
## this function creates a special matrix object that caches the inverse of its input
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL # indicates that we are assigning the NULL in the global environment
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## computes the inverse of the matrix returned by makeCacheMatrix above,
## if the inverse is not present in the cache memory.
## If the inverse matrix already exists in the cache memory then,
## it will fetch the matrix from cache instead of computing again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get() ##getting the matrix from the makeCacheMarix function
        inv <- solve(mat, ...) ##computing the inverse if is not present in cache memory
        x$setInverse(inv)
        invisible(inv)
}
