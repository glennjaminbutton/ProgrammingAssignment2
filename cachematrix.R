## Creates a special Matrix object that can be used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv_Mat <- NULL
    set <- function(y) {
        x <<- y
        Inv_Mat <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) Inv_Mat <<- inverse
    getInv <- function() Inv_Mat
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## The following function creates an inverse of the special matrix created by 
## the function named makeCacheMatrix(). If an inverse if already available 
## then one is not created and it is retrieve from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    Inv_Mat <- x$getInv()
    if(!is.null(Inv_Mat)) {
        message("getting cached data")
        return(Inv_Mat)
    }
    data <- x$get()
    Inv_Mat <- solve(data, ...)
    x$setInv(Inv_Mat)
    Inv_Mat
}
