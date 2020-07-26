## The two functions are used together where the first creates the object and the other cahes the inverse of the object, a matrix in this case. 

## This function creates an object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){ 
                x <<- y 
                inv <<- NULL 
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv} 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cache solve function computes the inverse of te special object. 

cachesolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}