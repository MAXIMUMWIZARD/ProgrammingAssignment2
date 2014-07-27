## Provides a function for creating a wrapper around a matrix that will cache
## its inverse when calculated with the cacheSolve function.

## Creates a list wrapper providing access to a matrix and its cached inverse

makeCacheMatrix <- function(mtrx = matrix()) {
	cached <- NULL
	set <- function(newMtrx) {
		mtrx <<- newMtrx
		cached <<- NULL
	}
	get <- function() mtrx
	setinverse <- function(inverse) cached <<- inverse
	getinverse <- function() cached
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of the matrix, caching the result on the first call and
## reusing that on all subsequent calls

cacheSolve <- function(cacheMtrx, ...) {
	cached <- cacheMtrx$getinverse()
	if (!is.null(cached)) {
		message("getting cached data")
		return(cached)
	}
	data <- cacheMtrx$get()
	cached <- solve(data, ...)
	cacheMtrx$setinverse(cached)
	cached
}
