

## The function makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:

## set the elements of the matrix
## get the elements of the matrix
## set the elements of the matrix inverse
## get the elements of the matrix inverse



makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special “matrix” 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets it in the cache via the setinverse function.




cacheInverse <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


## Example to test my code
test_matrix <- makeCacheMatrix(matrix(1:4,2,2))

test_matrix$get()
## return matrix(1:4,2,2)

test_matrix$getinverse()
## return NULL

cacheInverse(test_matrix)
## return the inverse of the matrix above

test_matrix$getinverse()
## return the inverse of the matrix above

cacheInverse(test_matrix)
## return getting cached data + the inverse of the matrix above


