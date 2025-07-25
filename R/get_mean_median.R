#' Function to get mean value
#'
#' Function for the arithmetic mean.
#'
#' @param x an R object. Numeric/logical vectors
#'
#' @return The mean of x
#' @export
#' 
#'
#' @examples
#' x <- c(1, 2)
#' getMean(x)
getMean <- function(x) {
  tryCatch({
    sum(x) / length(x)      
  }, error = function(e) {
    stop("Failed to calculate the mean of 'x': ", e$message)
  })
}

# returns TRUE if the length of x is an even number; FALSE otherwise
# isLengthAnEvenNumber
y <- function(x) {
  length(x) %% 2 == 0
}

# returns the median of x
getMedian <- function(x) {
  tryCatch({
    centerIndices <- ceiling(length(x) / 2)
    if (y(x)) {
      centerIndices <- c(centerIndices, centerIndices + 1)
    }
    getMean(sort(x)[centerIndices])            
  }, error = function(e) {
    stop("Failed to calculate the median of 'x': ", e$message)
  })
}