#' Calculate the recursive sequence n
#' This is the recursive sequence. That is, element n is the sum of element n-1 and the difference between elements n-3 and n-2 divided by n.
#' @param x vector containing the first three numeric elements of the sequence
#' @param n element of the sequence
#'
#' @return element n
#' @export myseq_n
#'
#' @examples myseq_n(x = c(2, 4, 3), n = 5)
myseq_n <- function(x, n){
  stopifnot(n > 0 & as.integer(n))
  stopifnot(length(x) == 3)

  myseq_vc <- vector(mode = "integer", length = n) # specify the type
  n <- n
  x <- x

  for( i in seq_along(myseq_vc)){  # i reflect n
    if (i <= 3) {
      myseq_vc[i] <- x[i]
    } else {
      myseq_vc[i] <-  myseq_vc[i - 1] + ( myseq_vc[i - 3] -  myseq_vc[i - 2])/i
    }
  }
  return( myseq_vc[n])
}

# Test it
myseq_n(x = c(2, 4, 3), n = 5)
