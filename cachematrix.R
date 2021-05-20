# Create a function that calculates the inverse of a cached matrix

# The function will be used to get and store the inverse of a matrix that is inputted into the function
#Function will be used set and get a matrix and then set and get the calculation of its inverse

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


# Calculate the inverse of the matrix or retrieve the inverse from the cache 
# The function will check to see if the the inverse has been calculated (to skip computation if it's done)
# Else, the function will calculate the inverse of the matrix


#i refers to the inverse
# matrix refers to the original matrix (ie. the dataset)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse
  if (!is.null(i)){
    #print message if there is a cached value
    message("get inverse matrix")
    #print inverse matrix from cache
    i
  }
  #If not in the cache, get the matrix 
  matrix <- x$get()
  #Calculate the inverse of the matrix
  i <- solve(matrix,...)
  #Set the inverse
  x$set_inverse(i)
  #Display the inverse
  i
}


#test functions
#using 5 x 5 matrix
sample_data<- matrix(rnorm(25),5,5)
matrix <- makeCacheMatrix(sample_data)
result <- cacheSolve(matrix)
result








