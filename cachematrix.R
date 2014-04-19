## This function creates a "matrix", which is really a list 
## containing a function that:
## 1.  sets the value of the matrix,
## 2.  gets the value of the matrix,
## 3.  sets the value of the inverse, and
## 4.  gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize empty inv variable; will eventually store the inverse of
    # the matrix
    inv <- NULL
    
    # set variable stores the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL   # reinitialize empty inv variable, matrix has changed
    }
    # get the value of the matrix
    get <- function() x
    # set the inverse
    setinv <- function(inv_) inv <<- inv_
    # get the inverse
    getinv <- function() inv
    
    # returns a list of all the above
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## The following function calculates the inverse of the above 
## matrix. First, the function checks whether the inverse has
## already been calculated.  If true, the inverse is retrieved 
## from the cache.
##
## If false, the inverse of the matrix is calculated and set
## in the cache.

cacheSolve <- function(x, ...) {
    
    # a check for whether the inverse is cached
    # if condition true, return the cached data, if false calculate inverse
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    } else {
        # set data variable to get the matrix
        data <- x$get()
        # calculate inverse
        inv <- solve(data, ...)
        # cache inverse in variable x
        x$setinv(inv)
        # return the inverse
        inv
    }
}