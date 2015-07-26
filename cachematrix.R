## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores in the cache the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL # the inverse matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x # get the matrix
        setinverese <- function(inverse) i <<- inverse # set the inverse value
        getinverse <- function() i # get the inverse value 
        list(set = set, get = get,
             setinverese = setinverese,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the input matrix. 
## If the inverse value is existing in the cache the function returns with the cached value, 
## otherwise load to the result to the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse() ## try to load the inverse matrix from the cache
        if(!is.null(i)) { ## if the cached value is not null
                message("getting cached data")
                return(i) ## return with the cached value
        }
		## if there was not cached value
        data <- x$get() ## get the matrix
        i <- solve(data, ...) ## Calculate inverse matrix
        x$setinverse(i)
        i ## Return a matrix that is the inverse of 'x'
}
