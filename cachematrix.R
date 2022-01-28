## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_ma <- NULL
    set_ma <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Define parameters
    get_matrix <- function() x
    setinverse <- function(inverse) inv_ma <<- inverse
    get_inverse <- function() inv_ma
    list(set = set_ma, 
         get_matrix = get_matrix,
         setinverse = setinverse,
         get_inverse = get_inverse)
}

# Testing makeCacheMatrix:
my_Matrix <- makeCacheMatrix(matrix(1:5, 4, 4))
my_Matrix$get_matrix()
my_Matrix$get_inverse()


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv_ma <- x$get_inverse()
    if(!is.null(inv_ma)) {
        return(inv_ma)
    }
    matrix_to_invert <- x$get_matrix()
    inv_ma <- solve(matrix_to_invert, ...)
    x$setinverse(inv_ma)
    inv_ma
}

# Testing fucntion:
cacheinverse(my_Matrix)
cacheinverse(my_Matrix)
