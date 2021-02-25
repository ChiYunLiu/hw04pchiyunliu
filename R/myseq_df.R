#' Calculate and plot the recursive sequence n using data frame
#'
#'This is the recursive sequence. That is, element n is the sum of element n-1 and the difference between elements n-3 and n-2 divided by n. This function uses data frame as input and return a plot as output.
#' @param my_data data frame
#'
#' @return plot element n
#' @export myseq_df
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip tibble
#' @examples
#' my_data <-tibble::tribble(~x, ~y, ~z, ~n,
#' 2,4,3,3,
#' 2,4,3,4,
#' 2,4,3,5,
#' 2,4,3,6,
#' 2,4,3,7,
#' 2,4,3,8,
#' 2,4,3,9,
#' 2,4,3,10,
#' 2,4,3,12)
#' myseq_df(my_data)
myseq_df <- function(my_data){
  stopifnot(my_data[[4]] > 0 & as.integer(my_data[[4]]))
  stopifnot(is.double(my_data[[1]]) & is.double(my_data[[2]]) & is.double(my_data[[3]]))


  input_val <- tibble::tibble(x = 0, y = 0)
  my_data <-  tibble::tibble(my_data)

  for (i in 1:nrow(my_data)){
    x = c(my_data[[i,1]], my_data[[i,2]], my_data[[i,3]])
    n = my_data[[i,4]]
    myseq_n(x, n) ->  input_val[i, 2]
    n ->  input_val[i, 1]
  }

  ggplot2::ggplot(input_val, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "n", y = "output", title = "My Sequence: c(3, 2.5, 2.7, 2.783, 2.755, 2.744, 2.748, 2.749, 2.748)") ->my_data_plot

  return(my_data_plot)

}

# Test it
my_data <- tibble::tribble(
  ~x, ~y, ~z, ~n,
  2,4,3,3,
  2,4,3,4,
  2,4,3,5,
  2,4,3,6,
  2,4,3,7,
  2,4,3,8,
  2,4,3,9,
  2,4,3,10,
  2,4,3,12)
myseq_df(my_data)
