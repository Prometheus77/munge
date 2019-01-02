#' @title munge
#' @description
#' Generates synthetic data which mimics joint distributions of original data.
#' See http://www.niculescu-mizil.org/papers/rtpp364-bucila.rev2.pdf. Algorithm
#' used in paper has been modified to use square root of hamming distance.
#'
#' @param data data frame to use as source for synthetic data generation
#' @param nn_ind (optional) pre-generated nearest neighbors index to use; if
#'   available, this will avoid the need to do the computationally intensive
#'   nearest neighbors calculation and save time
#' @param reps number of replications of data set required
#' @param p_swap Probability of swapping means or values between nearest
#'   neighbors. If a vector is passed, will draw from a uniform distribution
#'   between the minimum and maximum values.
#' @param local_var Local variance parameter that controls how. If a vector is
#'   passed, will draw from a uniform distribution between the minimum and
#'   maximum values.
#' @param as.discrete vector of columns to force to handle as discrete columns
#'
#' @export
munge <- function(data, reps = 10, p_swap = 0.5, local_var = 5, as.discrete = NULL, nn_ind = NULL) {

  continuous_cols <- which(sapply(data, class) %in% c("numeric", "integer", "double"))
  continuous_names <- names(data)[continuous_cols]

  discrete_cols <- which(!(sapply(data, class) %in% c("numeric", "integer", "double")))
  discrete_names <- names(data)[discrete_cols]

  if(length(continuous_cols) > 0) {
    data_continuous <- tibble::as.tibble(data[, continuous_cols])
    missing_continuous <- FALSE
  } else {
    data_continuous <- tibble::tibble(dummy_continuous = rep(0, nrow(data)))
    missing_continuous <- TRUE
  }

  if(length(discrete_cols) > 0) {
    data_discrete <- tibble::as.tibble(data[, discrete_cols])
    missing_discrete <- FALSE
  } else {
    data_discrete <- tibble::tibble(dummy_discrete = as.factor(rep("A", nrow(data))))
    missing_discrete <- TRUE
  }

  if(is.null(nn_ind)) {
    message("Computing nearest neighbors index")
    nn_ind <- nn_combined_distance(data_continuous, data_discrete)
  }

  output_discrete <- list()
  output_continuous <- list()

  for(i in 1:reps) {

    # randomize p_swap and local_var for iteration if ranges are entered
    p_swap_i <- min(p_swap) + (max(p_swap) - min(p_swap)) * runif(1)
    local_var_i <- min(local_var) + (max(local_var) - min(local_var)) * runif(1)
    message("Executing rep ", i," with p_swap=", round(p_swap_i, 3)," and local_var=", round(local_var_i, 3))

    #execute the munge algorithm one time for each rep

    #preserve shape and column heads
    data_rows <- nrow(data_continuous)
    data_cols_continuous <- length(continuous_cols)
    data_cols_discrete <- length(discrete_cols)

    #create nn matrices and vectorize
    if(is.null(data_cols_continuous)) {
      data_continuous_nn <- data_continuous[nn_ind]
    } else {
      data_continuous_nn <- c(as.matrix(data_continuous[nn_ind, ]))
      data_continuous <- c(as.matrix(data_continuous))
    }

    if(is.null(data_cols_discrete)) {
      data_discrete_nn <- data_discrete[nn_ind]
    } else {
      data_discrete_nn <- c(as.matrix(data_discrete[nn_ind, ]))
      data_discrete <- c(as.matrix(data_discrete))
    }

    #create swap masks
    swap_mask_continuous <- (runif(length(data_continuous)) < p_swap_i)
    swap_mask_discrete <- (runif(length(data_discrete)) < p_swap_i)
    swap_mask_as.discrete1 <- (data_continuous %in% as.discrete | data_continuous_nn %in% as.discrete) #find as.discrete values
    swap_mask_continuous <- (swap_mask_continuous & !swap_mask_as.discrete1) #remove as.discrete values from swap
    swap_mask_as.discrete <- (swap_mask_as.discrete1 & runif(length(data_continuous)) < p_swap_i) #randomize as.discrete swaps

    #swap continuous and draw from uniform distribution
    data_means <- data_continuous
    data_means[which(swap_mask_continuous)] <- data_continuous_nn[which(swap_mask_continuous)]
    data_sd <- abs(data_continuous - data_continuous_nn) / local_var_i
    data_sd[which(swap_mask_as.discrete1)] <- 0
    data_continuous <- rnorm(n = length(data_continuous), mean = data_means, sd = data_sd)

    #swap discretes (that was easy)
    data_discrete[which(swap_mask_discrete)] <- data_discrete_nn[which(swap_mask_discrete)]

    #swap "as.discretes", certain values to be treated as discrete variables (e.g. -9999999) (not yet built)
    data_continuous[which(swap_mask_as.discrete)] <- data_continuous_nn[which(swap_mask_as.discrete)]

    #return to matrix shape, make data frame, restore names
    if(is.null(data_cols_continuous)) {
      data_continuous <- as.data.frame(data_continuous)
      names(data_continuous) <- continuous_names
    } else {
      data_continuous <- as.data.frame(matrix(data_continuous, data_rows, data_cols_continuous))
    }
    names(data_continuous) <- continuous_names

    if(is.null(data_cols_discrete)) {
      data_discrete <- as.data.frame(data_discrete)
      names(data_discrete) <- discrete_names
    } else {
      data_discrete <- as.data.frame(matrix(data_discrete, data_rows, data_cols_discrete))
    }
    names(data_discrete) <- discrete_names

    output_discrete[[i]] <- data_discrete
    names(output_discrete[[i]]) <- discrete_names
    output_continuous[[i]] <- data_continuous
    names(output_continuous[[i]]) <- continuous_names
  }

  message("Munge operation completed: output contains ", nrow(data_discrete) * reps," rows")

  if(class(data) == "list") {
    out_data <- list(discrete = do.call(rbind, output_discrete), continuous = do.call(rbind, output_continuous))
    if(missing_continuous == TRUE) { out_data$continuous <- NULL }
    if(missing_discrete == TRUE) { out_data$discrete <- NULL }
    return(out_data)
  } else {
    out_data <- cbind(do.call(rbind, output_discrete), do.call(rbind, output_continuous))
    out_data <- out_data[,match(names(data), names(out_data))]
    return(out_data)
  }
}
