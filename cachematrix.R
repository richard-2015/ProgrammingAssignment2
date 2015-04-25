## The function can read matrix variable and calculate its inverse variable (provided the
## matrix is inversable. And it can cache previous calculation so if the matrix had been calculated,
## the outcome will be retrieved from cache.)



## This function has 4 functions inside.
## setdata(): assign value to variable
## getdata(): read value and stroe matrix value to variable
## setinversedata(): assign inversed matrix to variable
## getinversedata(): read inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        m1<-NULL
        
        setdata<-function(y){
                x<<-y  
                m1<<-NULL
                
        } 
        
        getdata<-function() x  
        
        setinversedata<-function(inver) m1<<- inver
        
        getinversedata<-function(){
                return(m1)
                
        } 
        
        list(getdata=getdata,setdata=setdata,
             setinversedata=setinversedata,
             getinversedata=getinversedata)   

}


## Write a short comment describing this function
## This function calcuates inverse matrix of given variable. Also caches previous calculated inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinversedata()
        
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
                
        }
        newdata <- x$getdata()
        inver_outcome <- solve(newdata, ...)
        x$setinversedata(inver_outcome)
        inver_outcome
}
