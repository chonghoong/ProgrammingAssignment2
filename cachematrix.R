## makeCacheMatrix and cacheSolve calcuate the inverse of a matrix and stores it as a cache inverse. 


## Returns a list containing set and get functions of the matrix object and set and get functions of the inverse of the matrix object
makeCacheMatrix <- function(m = matrix()) {
        
        ## variable to store the inverse of the matrix object
        i <- NULL
        
        ## set the matrix object and set/reset its inverse to NULL
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        
        ## return the matrix
        get <- function() m
        
        ## set the inverse of the matrix (defined in the scope of makeCacheMatrix function)
        setMatrixInverse <- function(matrixInverse) i <<- matrixInverse
        
        ## return the inverse of the matrix
        getMatrixInverse <- function() i
        
        ## return a list of containing the 4 functions
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}









## Returns the cached inverse of the matrix in 'x' if there is. 
## Else, this function calculates the inverse and stores it in 'x' as a cached value

cacheSolve <- function(x, ...) {
        
        ## get the inverse of the matrix object in 'x'
        i <- x$getMatrixInverse()
        
        ## check if the inverse of the matrix object in 'x' is null
        if(!is.null(i)) {
                
                ## ouput a message to indicate that there is a cached inverse of the matrix object in 'x'
                message("getting cached data")
                
                ## return the cached inverse of the matrix object in 'x'
                return(i)
        }
        
        
	## get the matrix object in 'x'
        data <- x$get()
      
        ## calculate the inverse of the matrix in 'x' and store in i
        i <- solve(data, ...)
        
        ## set the inverse  of the matrix in 'x'
        x$setMatrixInverse(i)
        
        ## return the inverse of the matrix in 'x'
        i
}