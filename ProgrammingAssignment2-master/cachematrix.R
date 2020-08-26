## Tony Johnson
## 8/25/2020
## These functions enable a user to cache solved inverse matrices
## into the R environment so that
## if the inverse was solved, the program will automatically
## call the inverse from the cache
## Code starter used was from: https://github.com/rdpeng/ProgrammingAssignment2


## Usage: let A be a square, invertible matrix.
## Step 1: Cache the matrix A. I.e: B<-makeCacheMatrix(A)
## Step 2: Solve the inverse of the matrix A
##          Using cacheSolve(B)
## Step 3: Try to solve for the inverse again using cacheSolve(B)
## Step 3 will result in the console printing out "getting cached data"

## makeCacheMatrix is used by cacheSolve to create a list containing
## a function to set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve solves the inverse of some matrix from makeCacheMatrix
## if the inverse has not been solved, it will solve it and store it
## if the inverse has been solved and is stored, 
## it will call the value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

