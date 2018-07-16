## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse,
## which is a list containing a function to: 
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse matrix
## 4 get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setinverse(inver)
        inver
        ## Return a matrix that is the inverse of 'x'
}

## Example: if a <- matrix(1:4,2,2), then the inverse of matrix a is a- <- matrix(c(-2,-1,1.5,-0.5),2,2).
## Here is the running output of this example in R Console:
## > source("R programming assignment2.R")
## > a <- matrix(1:4, 2, 2)
## > b <- makeCacheMatrix(a)
## > b$get()
##       [,1] [,2]
## [1,]    1    1
## [2,]    2    2
## > b$getinverse()
## Null
## First time run cacheSolve(b), because now inver <- Null, so cacheSolve() compute the inverse matrix and
## cache it.
## > cacheSolve(b)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Now the inverse matrix is caching
## > b$getinverse()
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## and then run cacheSolve(b) again, because now inver is caching, so we get the return inver:
## getting cached data
## > cacheSolve(b)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


