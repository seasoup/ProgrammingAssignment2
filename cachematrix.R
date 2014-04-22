## These functions will cache a matrix and its inverse in order to speed up computations

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	matrix <- x
	cachedMatrix <- NULL
	
	setInverseMatrix <- function(m) {
		x <<- m
		cachedMatrix <<- solve(x)
	}
	
	getInverseMatrix <- function() {
		cachedMatrix
	}
	
	list(
		setInverseMatrix = setInverseMatrix,
		getInverseMatrix = getInverseMatrix
	)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. If the cache has not yet been set, it caches the inverse matrix. The cached result is then returned.  If the cache has already been set, it just returns the cached copy.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- makeCacheMatrix(x)

        if ( is.null(cache$getInverseMatrix()) ) {
        	cache$setInverseMatrix(x)
        }

        cache$getInverseMatrix()
}
