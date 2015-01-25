makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    ##creating the checker whether we have new (having NULL) or old value of matrix which inverse we want to calculate
        set <- function(y) { ## create function that sets a new value of matrix
                x <<- y
                m <<- NULL ## assigns NULL since we have a new value of a matrix
        }
        get <- function() x ## getting a value of a matrix
        setInverse <- function(solve) m <<- solve ## setting a value of a inverse matrix
        getInverse <- function() m ## getting a value of a inverse matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) ## return list of objects
}

cacheSolve <- function(x, ...) { 
        m <- x$getInverse() ## getting a values of inverse matrix from x 
        if(!is.null(m)) { ## checking whether the inverse is not already in cache
                message("getting cached data") ## 
                return(m) ## printing old, but still valid, value of the inverse matrix
        } ## if not true the condition above, we will calculate a new value of inverse 
        data <- x$get() ## getting matrix from x
        m <- solve(data, ...) ## calculating an inverse of the matrix data
        x$setInverse(m) ## setting up an inverse to x
        m ## printing the new inverse matrix
}