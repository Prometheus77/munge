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
