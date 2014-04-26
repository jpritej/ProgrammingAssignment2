## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss 
## here). The next functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               #initialized inverse = NULL
        set <- function(y) {                    #define set() function
                x <<- y                         #assign argument
                m <<- NULL                      #reset inverse
        }
        get <- function() x                     #define get() function    
        setinverse <- function(inverse) m <<- inverse   #define setinverse()
        getinverse <- function() m              #define getinverse() function
        list(set = set, get = get,              #return the special "matrix"
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()        #query the x vector's cache 
        if(!is.null(m)) {          #if there is a cache 
                message("getting cached data")
                return(m)          #just return the cache, no computation needed
        }
        data <- x$get()            #if there's no cache
        m <- solve(data, ...)      #we actually compute them here
        x$setinverse(m)            #save the result back to x's cache
        m                          #return the result
}
