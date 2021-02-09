determine_passing_spec <- function(values, lower_threshold = NULL, upper_threshold = NULL, digits_round = NULL, pass_at_threshold = TRUE){
  assertthat::assert_that(mode(values) == "numeric")
  assertthat::assert_that(length(values) >= 1)

  if (is_null(digits_round)){
    values_round <- values
  } else {
    values_round <- round(values, digits_round)
  }

  qc_flags <- logical(length(values))

  if (!is.null(lower_threshold)){
    if (pass_at_threshold){
      i_low <- which(values_round <= lower_threshold)
    } else {
      i_low <- which(values_round < lower_threshold )
    }

    if (length(i_low) > 0){
      qc_flags[i_low] <- TRUE
    }
  }

  if (!is.null(upper_threshold)){
    if (pass_at_threshold){
      i_high <- which(values_round >= upper_threshold)
    } else {
      i_high <- which(values_round > upper_threshold )
    }

    if (length(i_high) > 0){
      qc_flags[i_high] <- TRUE
    }
  }

  return(qc_flags)

}

