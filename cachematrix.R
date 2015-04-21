
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    } 
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


# This returns a matrix that is the inverse of x
# it will print the cached inverse if inverse already exists for the same matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinv(m)
    m
}
