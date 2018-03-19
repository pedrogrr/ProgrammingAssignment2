## This function gets a matrix and return a list with this functions
## set the matrix, get the matrix, set the inverse, get the inverse

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                ##in this case we assign a value to x in a different environment
                x <<- y 
                ##in this case we assign NULL to y in a different environment
                m_inv <<- NULL 
        }
        get <- function() x
        setinverse <- function(inverse) m_inv <<- inverse
        getinverse <- function() m_inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function returns the inverse matrix obtained from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinverse()
        if (!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinverse(m_inv)
        m_inv
}
