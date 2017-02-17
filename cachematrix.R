## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creating a special vector that will be used to make calls for a result.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(invMatrix) m <<- invMatrix
        getInvMatrix <- function() m
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function
#Below fuction computes the matrix inverse for a special vectors.Also, if result is already compute, it will return it without computing again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMatrix(m)
        m
}
