#
## Gerardo Javier Alanis Funes
#
## Functions: makeCacheMatrix
#             cacheSolve


# makeCacheMatrix: creates a matrix object and associated functions.

makeCacheMatrix <- function(x = matrix()) {
        
        inverseMat <- NULL
        
        set <- function(y) {
                x <<- y
                inverseMat <<- NULL
        }
        
        get <- function() {
                x
        }
        ## Compute the inverse
        setinverse <- function(solve) {
                inverseMat <<- solve
        }
        getinverse <- function() {
                inverseMat
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve: Compute the inverse matrix if it is not calculated or 
#             get the matrix from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverseMat <- x$getinverse()
        if(!is.null(inverseMat)) {
                message("getting cached data")
                return(inverseMat)
        }
        matrix <- x$get()
        inverseMat <- solve(matrix, ...)
        x$setinverse(inverseMat)
        inverseMat

}

