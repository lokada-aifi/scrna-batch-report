#' Determine passing specification
#'
#' This function is currently unused. Confirm functionality and modify if needed before use.
#'
#' @param values Numeric vector of values to evaluate for spec
#' @param lower_threshold Numeric value that represents the lower limit of acceptable values
#' @param upper_threshold Numeric value that represents the upper limit of acceptable values
#' @param digits_round The number of digits that values are to be rounded to before evaluation
#' @param pass_at_threshold Logical value. Whether or not values equal to threshold should pass, defaults to TRUE.
#' @param flag_values Vector of length 2 where the first value is the value to be returned for passing/non-flagged values
#' and the second value is the value to be returned for non-passing/flagged values.
#' @return Vector of length equal to input values. Output types defined by flag_values parameter
#' @export
#' @examples
#' myvalues <- 1:20
#' flags <- determine_passing_spec(myvalues, lower_threshold = 5, upper_threshold = 18, pass_at_threshold = TRUE)
#' flags
#' flags2 <- determine_passing_spec(myvalues, lower_threshold = 5, upper_threshold = 18, pass_at_threshold = FALSE)
#' flags2
determine_passing_spec <- function(values, lower_threshold = NULL, upper_threshold = NULL, digits_round = NULL, pass_at_threshold = TRUE, flag_values = c(FALSE,TRUE)){
  assertthat::assert_that(mode(values) == "numeric")
  assertthat::assert_that(length(values) >= 1)

  if (is_null(digits_round)){
    values_round <- values
  } else {
    values_round <- round(values, digits_round)
  }

  qc_flags <- rep(flag_values[1],length(values))

  # Flag values < or <= threshold
  if (!is.null(lower_threshold)){
    if (pass_at_threshold){
      i_low <- which(values_round < lower_threshold)
    } else {
      i_low <- which(values_round <= lower_threshold )
    }

    if (length(i_low) > 0){
      qc_flags[i_low] <- flag_values[2]
    }
  }

  # Flag values > or >= threshold
  if (!is.null(upper_threshold)){
    if (pass_at_threshold){
      i_high <- which(values_round > upper_threshold)
    } else {
      i_high <- which(values_round >= upper_threshold )
    }

    if (length(i_high) > 0){
      qc_flags[i_high] <- TRUE
    }
  }

  return(qc_flags)

}

