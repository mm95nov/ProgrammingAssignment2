## Calculates inverse of an invertible matrix which is ##then stored in the cache
##so the user does not have to call the solve function every time the user would like 
##to invert the given matrix

##Caches the results of the solve function
makeCacheMatrix <- function(x=matrix()){
	m<-NULL
	set<-function(y){
		x <<- y
		m <<- NULL
	}
	get<-function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set=set, get=get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## Calls the cache for results of inverted matrix

cacheSolve <-function(x,...){
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
