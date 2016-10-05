## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of inverse of the matrix
## 4.get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
        inver <- NULL
        set <- function(y)
        {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix. It first checks if 
## the inverse has already been computed. If not, it computes the inverse of the
## Matrix, sets the value in the cache via setinverse function. If the inverse 
## of Matrix has already been computed then it gets the result and skips the 
## computation.

cacheSolve <- function(x, ...)
 {
        inver <- x$getinverse()
        if(!is.null(inver))
        {
                message("Getting Cached Data.")
                return(inver)
        }
        Matrixdata <- x$get()
        inver <- solve(Matrixdata)
        x$setinverse(inver)
        inver
}