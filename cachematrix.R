## This two functions used togheter are capable of storing 1 cached element so 
#it don't have to be calculated again


## This function works as an index in which the following function relies on


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}




##Verify m has allready been calculated, if so will print and end the function,
#if not the data will be calculated and printes,


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    print(m)  
}


b <- makeCacheMatrix(matrix(c(1,2,3,4),ncol=2, nrow=2))
b$set()
b$get()
b$getinverse()
b$setinverse()
cacheSolve(b)

