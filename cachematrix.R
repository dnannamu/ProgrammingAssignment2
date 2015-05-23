## The below two functions calculate and cache the inverse of a matrix. If the inverse is already cached, inverse is not recomputed.
## Assumption: Matrix is always invertible.


## This function returns a list that contains functions to set the value of the matrix, get the value of the matrix, set the value of inverse of ##the matrix and get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {


        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## This function returns inverse of the matrix. If inverse is already computed, it gets the result and computation is skipped. If not, it ##computes the inverse and sets the value of inverse in cache.

cacheSolve <- function(x, ...) {
        
	inv <- x$getinverse()

        if(!is.null(inv))
	 {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        message("inverse first time")
        inv
}