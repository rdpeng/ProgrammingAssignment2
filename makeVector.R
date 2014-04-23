#thedata <-c(1.1, 1.2, 2.1, 2.2)  ## passing a vector
#x <- matrix(thedata, 2, 2)


makeVector <- function(x = thedata) { ## Passing vector thedata
  m <- NULL
  set <- function() {
    x<<- y

      m<<-NULL

  }

  get <- function() x
  setmean <- function(mean)
    m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
