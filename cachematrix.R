## These functions provide caching mechanism for matrix inversion, saving
## computational power by calculating matrix inverse only once.
## IMPORTANT: as per instructions it is assumed the matrix has a valid inversed matrix
##
## Usage examples:
## 1) To create a cached matrix:
##    testMatrix <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2, byrow=TRUE))
## 2) To obtain a matrix inverse from a cached matrix:
##    cacheSolve(testMatrix)
## 3) To get values from a cached matrix:
##    testMatrix$get()
## 4) To set values in a cached matrix:
##    testMatrix$set(matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3, byrow=TRUE))
##



## makeCacheMatrix creates a cached matrix
## Usage examples:
## 1) To get values from a cached matrix:
##    testMatrix$get()
## 2) To set values in a cached matrix:
##    testMatrix$set(matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3, byrow=TRUE))

makeCacheMatrix <- function(x = matrix()) {
    # Cached inversed matrix
    inversed_matrix <- NULL
    
    # Set
    set <- function(y) {
        x <<- y
        inversed_matrix <<- NULL
    }
    
    # Get
    get <- function(){
        x
    }
    
    # Set inverse, internal
    setinverse <- function(inverse){
        inversed_matrix <<- inverse
    }
    
    # Get inverse, internal
    getinverse <- function(){
        inversed_matrix
    }
    
    # Methods wrapper
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## cacheSolve uses mechanism provided by cached matrix to reduce computational
## time of inversing matrix.
##
## Assuming you have created a cached metrix, using makeCacheMatrix, obtaining
## the matrix inverse is done through calling cacheSolve on this object.
## Note: when a cached value is return, the user is informed through a message
##
## Usage example:
##    testMatrix <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2, byrow=TRUE))
##    cacheSolve(testMatrix) # Inverse calculated for the first time
##    cacheSolve(testMatrix) # No computation required, cached value returned
##

cacheSolve <- function(x, ...) {
    
    # Check if we can use cached value and avoid computation
    inversed_matrix <- x$getinverse()
    if (!is.null(inversed_matrix)) {
        message("getting cached matrix inverse")
        return(inversed_matrix)
    }
    
    # Looks like inversed matrix wasn't computed yet, let's do that...
    original_matrix <- x$get()
    inversed_matrix <- solve(original_matrix)
    
    # ...store it for future retrieval, and return the value
    x$setinverse(inversed_matrix)
    inversed_matrix
}
