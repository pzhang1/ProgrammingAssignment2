## This file contains two functions for cahcing the 
## inverse of matrix. 

## This function creates a list containing a special
## matrix object with set matrix values, get matrix
## values, set the matrix inverse and get the matrix
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## This function calculates the inverse of matrix crated
## in the above function, it checks to see if the inverse
## of matrix is already calculated, if so, it gets the inverse
## from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the inverse
## of matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv        
}
