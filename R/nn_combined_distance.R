#' @title nearest neighbors combined distance
#' @description
#' Determines the nearest neighbors distance between each pair of rows by
#' computing the Euclidean distance of continuous variables (scaled from 0-1
#' based on min/max) and the square root of the Hamming distance for discrete
#' variables.
#'
#' @param continuous_data data frame containing all continuous variables
#' @param discrete_data data frame containing all discrete variables
#' @param batchsize subdivide data into batches of this size in order to speed up
#' computation. Set to zero to force all data into a single batch (will be
#' extremely slow for large datasets).
#'
#' @export
nn_combined_distance <- function(data, batchsize = 2000) {
  data_split <- split_data(data)
  nn_combined_distance_split(continuous_data = data_split$continuous,
                             discrete_data = data_split$discrete,
                             batchsize = batchsize)
}

nn_combined_distance_split <- function(continuous_data, discrete_data, batchsize = 2000) {
  if(batchsize == 0 || batchsize >= nrow(continuous_data)) {
    output <- nn_combined_distance_iteration(continuous_data, discrete_data)
  } else {
    batches <- nrow(continuous_data) %/% batchsize
    if(nrow(continuous_data) %% batchsize > 0 ) {batches <- batches + 1}
    batchkey <- sample(1:batches, nrow(continuous_data), replace=TRUE)
    output <- rep(0, nrow(continuous_data))
    for(i in 1:batches) {
      thisbatch <- which(batchkey == i)
      batchoutput <- nn_combined_distance_iteration(continuous_data[thisbatch, ], discrete_data[thisbatch, ])
      output[which(batchkey == i)] <- thisbatch[batchoutput]
    }
  }
  return(output)
}


nn_combined_distance_iteration <- function(continuous_data, discrete_data)
{
  #returns the nearest neighbor combining euclidean and hamming distance

  dist_eucl <- as.matrix(dist(data.matrix(standardize_linear(continuous_data)), method = 'euclidean'))
  dist_hamm <- hamming(data.matrix(discrete_data))
  dist_comb <- sqrt(dist_eucl) + dist_hamm

  #create an array with the nearest neighbor index for each row
  nn_col <- array(1:ncol(dist_comb))

  for(i in 1:nrow(dist_comb))
  {

    #sort the row, the 1st value should be the current row, the 2nd is the
    #nearest neighbor

    index_value <- order(dist_comb[i,])[2]

    #if for some reason (tie) the algorithm chooses the current row as nearest
    #neighbor, force it to choose the 1st value instead

    if(index_value == i)
    {
      nn_col[i] <- order(dist_comb[i,])[1]
    } else {
      nn_col[i] <- index_value
    }
  }
  return(nn_col)
}
