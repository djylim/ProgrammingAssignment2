#The idea of this function is to help compute matrix inverses in a more efficient manner
#Typically using the solve function by itself can be quite computationally intensive
#The function is designed to cache a matrix inverse such that R can read from it later on if the matrix already exists


#A key consideration is that the matrix should be square
#This is because solve can only invert square matrices

#The function below requires you enter a matrix (created using normal matrix creation syntax)

#From the instructions, the function does
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will cache invere
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #The if statement checks if a cached version already exists, if not it uses solve, if it does it returns the cached copy
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#Just a quick example to show it works
#> testmatrix<-makeCacheMatrix(matrix(2:5,2,2))
#> cacheSolve(testmatrix)
#[,1] [,2]
#[1,] -2.5    2
#[2,]  1.5   -1