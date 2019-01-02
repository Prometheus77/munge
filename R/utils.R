standardize_linear <- function(input_data,min_list=sapply(input_data,min),max_list=sapply(input_data,max)) {
  for(i in 1:ncol(input_data)) {
    x <- as.vector(input_data[,i])
    if(max_list[i] > min_list[i]) {
      standardized_array <- ((x-min_list[i])/(max_list[i]-min_list[i]))
    } else {
      standardized_array <- rep(0,nrow(input_data))
    }
    if(i==1) {
      output_data <- data.frame(standardized_array)
      colnames(output_data)[1] <- colnames(input_data)[1]
    } else {
      output_data[colnames(input_data)[i]] <- standardized_array
    }
  }
  return(output_data)
}

hamming <- function(input_matrix) {

  #returns a matrix of hamming distance of each row in an input matrix with the other rows
  #each cell shows the number of members between the two rows which are not identical

  n <- nrow(input_matrix)
  m <- matrix(nrow=n, ncol=n)
  for(i in seq_len(n))
    for(j in seq(i, n))
      m[j, i] <- m[i, j] <- sum(input_matrix[i,] != input_matrix[j,])
  return(m)
}

split_data <- function(input_data) {
  continuous_cols <- which(sapply(input_data, class) %in% c("numeric", "integer", "double"))
  continuous_names <- names(input_data)[continuous_cols]

  discrete_cols <- which(!(sapply(input_data, class) %in% c("numeric", "integer", "double")))
  discrete_names <- names(input_data)[discrete_cols]

  if(length(continuous_cols) > 0) {
    data_continuous <- tibble::as.tibble(input_data[, continuous_cols])
    missing_continuous <- FALSE
  } else {
    data_continuous <- tibble::tibble(dummy_continuous = rep(0, nrow(input_data)))
    missing_continuous <- TRUE
  }

  if(length(discrete_cols) > 0) {
    data_discrete <- tibble::as.tibble(input_data[, discrete_cols])
    missing_discrete <- FALSE
  } else {
    data_discrete <- tibble::tibble(dummy_discrete = as.factor(rep("A", nrow(input_data))))
    missing_discrete <- TRUE
  }

  list(continuous = data_continuous,
       discrete = data_discrete,
       continuous_cols = continuous_cols,
       continuous_names = continuous_names,
       discrete_cols = discrete_cols,
       discrete_names = discrete_names)
}
