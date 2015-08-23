# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix())
{
    
    i <- NULL
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) i <<- inverse
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

cacheSolve <- function(x, ...)
{
    i <- x$getInv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInv(i)
    i
}
