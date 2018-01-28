## This code contains tow main functions, which are makeCacheMatrix and cacheSolve. Based on these two functions, this version of
## code could perform two achievements.
## 1: Creating a special "matrix" object that can cache its inverse.
## 2: Computing the inverse of the special "matrix" returned by the first function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        temp_matrix <- NULL
        set <- function(y) {
                x <<- y
                temp_matrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) temp_matrix <<-inverse
        getInverse <- function() temp_matrix
        list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## Computing the inverse of the special "matrix" returned by the first function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        temp_matrix <- x$getInverse()
        if(!is.null(temp_matrix)) {
                message("getting cached data")
                return(temp_matrix)
        }
        data <- x$get()
        temp_matrix <- solve(data, ...)
        x$setInverse(temp_matrix)
        temp_matrix
}
