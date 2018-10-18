## This function makes a list of 4 functions (set, get, setinverse, getinverse).
## With these four function you are able to set or retrieve the matrix,
## and to set or retrieve the inverse of this matrix, (once it is calculated by cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function expects as an input a list of the type outputted by makeCacheMatrix.
## If the list already contains the inverse of a matrix, it will return this inverse.
## If not, it will calculate the inverse, set the inverse in the list and return the inverse

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
