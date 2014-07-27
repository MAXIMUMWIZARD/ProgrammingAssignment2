## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(cacheMtrx, ...) {
        ## Return a matrix that is the inverse of 'x'
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
