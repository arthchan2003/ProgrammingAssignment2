## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Model after makeVector. This function creates
## the interface for a cached Matrix, including
## 4 functions, set, get, setinverse and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL		
	set <- function(y = matrix()) {
	    x <<- y
	    inv <<- NULL
	}

	get<-function() x
	setinverse <- function(inverse)  inv <<- inverse
	getinverse <- function () inv
	list (set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Write a short comment describing this function This functions cache
## the inverse of a matrix after the first request.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
#	print data
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
