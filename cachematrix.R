
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

## This function computes the inverse value of the special "matrix" created by 
## makeCacheMatrix, which we have coded above. If the inverse value has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache; otherwise, the cacheSolve 
## function conducts the calculation itself.
cacheSolve <- function(x, ...){
        
        inv <- x$getinverse()
        
        if(!is.null(inv)){
                message("getting cashed data")
                return(inv)
        }
        
        mat <- x$get()
        
        inv <- solve(mat, ...)
        
        x$setinverse(inv)
        
        inv
        
}

