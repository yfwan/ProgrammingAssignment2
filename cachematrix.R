## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL
 	set <- function(y) {
 		x <<- y
 		inv <<- NULL
 	}
 	get <- function()x
 	setinverse <- function(solve) inv <<- inverse
 	getinverse <- function() inv
 	list(set = set, get = get,
 		setinverse = setinverse,
 		getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
        ## Return a matrix that is the inverse of 'x'
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setinverse(inv)
	inv 
}
