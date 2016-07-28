# These functions are used to calculate inverse of a square matrix. The result is cached when
# it gets calculated.
# Usage:
#   - Create a cache object for matrix using 
#     mycache <- makeCacheMatrix( mymatrix )
#   - Get the inverserse with
#     myinverse <- cacheSolve( mycache )
# Example:
#   > m2x2 <- matrix( 1:4, 2, 2 )
#   > cache2x2 <- makeCacheMatrix( m2x2 )
#   > cacheSolve( cache2x2 )


# Creates a cache matrix object which is used by cacheSolve().
# Param x ... A square matrix that will be inversed by cacheSolve()
# Returns a list with internal representation of the object.
makeCacheMatrix <- function( x = matrix() ) {
    inversedX <- NULL
    set <- function( y ) {
        x <<- y
        inversedX <<- NULL
    }
    get <- function() x
    setInverse <- function( inv )  {
        inversedX <<- inv
    }
    getInverse <- function() {
        inversedX
    }
    list( set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse )
}


# Get the inverse of a cache matrix object.
# Param x ... A cache matrix object created by makeCacheMatrix()
# Returns a matrix that is the inverse of 'x'.
cacheSolve <- function( x, ... ) {
    inversedData <- x$getInverse()
    if( !is.null( inversedData ) ) {
        message( "using cached data" )
        return( inversedData )
    }
    message( "not in cache, calculating inversion" )
    data <- x$get()
    inversedData <- solve( data )#, ... )
    x$setInverse( inversedData )
    inversedData
}
