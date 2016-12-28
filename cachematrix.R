## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        inv_els=NULL
        set <- function(y){
                x <<-y
                inv_els <<- NULL
        }
        get <- function() x
        setInverse<- function(Inverse) inv_els <<-Inverse
        getInverse <- function() inv_els
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_els <- x$getInverse()
        if(!is.null(inv_els)) {
                message("Get cache data for Elsa")
                return(inv_els)
        }
        else{
                data <- x$get()
                inv_els <- solve(data)
                x$setInverse(inv_els)
                inv_els
        }
}

