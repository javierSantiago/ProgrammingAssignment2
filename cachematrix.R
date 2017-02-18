## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This funciton creates a special vector of function to handle information of a given Matrix.

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
#Below fuction computes the matrix inverse for a special vector produce with "makeCacheMatrix" function.
#If result is already computed, function will returned the stored result, otherwise it will compute it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #getting stored result.
        m <- x$getInvMatrix() 
        #Null means that i has not been computed previously. 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        #Function to compute the inverse of a given matrix
        m <- solve(data, ...)
        #Storing result for future referencees
        x$setInvMatrix(m)
        m
}
