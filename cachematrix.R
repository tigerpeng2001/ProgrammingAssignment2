## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix makes a list of four functions to manipulate tow
# variables x - the matrix and i - its inverse in the function
# makeCacheMatrix's clouse. 
#
#  1. set the value of the matrix, and nullize the inverse;
#  2. get the value of the matrix;
#  3. set the value of the inverse;
#  4. get the value of the inverse;

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # get the inverse
        i <- x$getinv()
        # if x's inverse is set (cached)
        if(!is.null(i)) { 
                message("getting cached data")
                ## Return the cached inverse of 'x'
                return(i)
        }
        # lese: x's inverse is not set (or has been reset as matrix has been changed)
        # get the matrix
        data <- x$get()
        # calculate its inverse
        i <- solve(data, ...)
        # cache the inverse
        x$setinv(i)
        # return the newly calculated and cached inverse
        i
}
