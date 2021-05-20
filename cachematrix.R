# Create a function that calculates the inverse of a cached matrix

#Set and get matrix and inverse


# x refers to the matrix
# i refers to the inverse
makeCacheMatrix <- function(x = matrix()) {
#create variable for the inverse
  i <- NULL
  #set the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  
  #set the inverse of the matrix
  #use solve() to calculate the inverse
  set_inverse <- function(solve) i <<- solve
  get_inverse <- function() i
  
  list(set=set,
       get = get,
       set_inverse = set_inverse,
       get_inverse= get_inverse)
  
}


# Calculate the inverse of the matrix  
# 1 - Check to see if the the inverse has been calculated (to skip computation if it's done)
# Else, calculate inverse


#i refers to the inverse
# matrix refers to the original matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse
  if (!is.null(i)){
    #print message if there is a cached value
    message("get inverse matrix")
    #print inverse matrix from cache
    i
  }
  matrix <- x$get()
  i <- solve(matrix,...)
  x$set_inverse(i)
  i
}


#test functions
#using 5 x 5 matrix
sample_data<- matrix(rnorm(25),5,5)
matrix <- makeCacheMatrix(sample_data)
result <- cacheSolve(matrix)
result








