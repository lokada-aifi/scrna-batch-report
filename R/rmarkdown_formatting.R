# Rmarkdown formatting helper functions

#' Knit a plot as a subchunk
#'
#' Knit a plot as a subchunk so its dimensions can be specified individually
#'
#' Allows individual plots within the same chunk to be knit as subchunks with
#' unique chunk options. Plots can be output in a loop with each plot using
#' different dimensions, ie dynamic dimensions based on number of x and/or y
#' category levels. Parent chunk should have options 'results = "as-is"'
#'
#' @param g The plot object
#' @param subchunk_name Unique name of Rmarkdown subchunk to be generated
#' @param chunk_opt_list Named list of chunk options for the subchunk. Can take any chunk
#' options available to a normal chunk.
#' @examples
#' g_example <- ggplot(df(x=1:10, y = 1:10)) + geom_point()
#' chunk_opt_l <- list(fig.height=10, fig.width=12, warning=TRUE)
#' make_subchunk(g_example, "test_chunk", chunk_opt_list = chunk_opt_l)

make_subchunk <- function(g, subchunk_name, quiet_knit = TRUE, chunk_opt_list = list(fig.height=7, fig.width=5, warning = TRUE)) {
  if(is.null(subchunk_name)){
    subchunk_name <- paste0(as.numeric(Sys.time()), "_",)
  }

  g_deparsed <- paste0(deparse(
    function() {g}
  ), collapse = '')

  # construct chunk option string
  if(!is.null(chunk_opt_list)){
    option_names <- names(chunk_opt_list)
    option_string_list <- sapply(1:length(chunk_opt_list), function(i){
      val <- chunk_opt_list[[i]]
      val_type <- class(val)
      quote_string <- ifelse(val_type=="character","'","")
      val_fmt <- paste0(quote_string, val, quote_string)
      paste(names(chunk_opt_list)[i], val_fmt, sep = "=")
    })
    option_string <- paste(c(", ",option_string_list), collapse = ", ")
  } else {
    option_string <- ""
  }

  # construct full chunk
  sub_chunk <- paste0("\n```{r ", subchunk_name, option_string, "}",
                      "\n(", g_deparsed, ")()",
                      "\n```")

  # knit chunk
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = quiet_knit))
}

#' Create a simple HTML table of key and value pairs
#'
#' Create a simple table of paired values (ie name and value).
#'
#' @param labels Vector of labels
#' @param values Vector of values, same length as labels
#' @param col_widths_px Numeric vector of length 2. Widths in pixels of each column,
#' @param fontsize Numeric value. Default 2.
#' @export
#' @return HTML of formatted table.
#' @examples
#' lab1 <- c("Batch", "N Samples", "N Pools")
#' val1 <- c("B000", 16, 2)
#' h_tab <- simple_html_table(lab1, val1)
#' htmltools::html_print(h_tab)
simple_html_table <- function(labels, values, col_widths_px = c(300, 800), fontsize = 2){
  assertthat::assert_that(length(col_widths_px) %in% c(1,2))
  assertthat::assert_that(length(labels) == length(values))
  assertthat::assert_that(all(col_widths_px > 100) & all(col_widths_px < 1024))
  # assertthat::assert_that(ifelse(is.null(col.names),TRUE, length(col.names) == 2))
  # assertthat::assert_that(is.null(col.names) | class(col.names) == "character")

  if(length(col_widths_px) == 1){
    col_widths_px <- c(col_widths_px, col_widths_px)
  }

  create_row <- function(value1, value2 , col_widths_px = c(50, 50)){
    sprintf('<tr>
                <td style="width:%spx">
                  %s
                </td>
                <td style="width:%spx">
                  %s
                </td>
            </tr>',
            col_widths_px[1], value1, col_widths_px[2], value2)
  }

  add_table_wrap <- function(all_row_string, fontsize = 2, col_widths_px= c(50,50)){
    sprintf('<font size="%s">
            <style>
              table {
                border: 1px solid black;
                border-collapse: collapse;
              }
              td {
                padding: 5px;
                border: 1px solid black;
                border-collapse: collapse;
              }
            </style>
            <table>
              <colgroup>
                <col span="1" style="width: %spx;">
                <col span="1" style="width: %spx;">
              </colgroup>
              <tbody>
                %s
              </tbody>
            </table>
            </font>', fontsize, col_widths_px[1], col_widths_px[2], all_row_string)
  }

  row_strings <- character()
  for (i in seq_along(labels)){
    row_strings[i] <- create_row(labels[i], values[i], col_widths_px = col_widths_px)
  }
  row_string_all <- paste(row_strings, collapse = "")

  out_table <- add_table_wrap(row_string_all,fontsize = fontsize, col_widths_px = col_widths_px )

  htmltools::HTML(out_table)
}



gt_fmt_comments <- function(x) {
  x %>%
    gt::tab_style(
      style = list(
        cell_fill(color = "red" , alpha = 0.3)
      ),
      locations = cells_body(
        columns = vars(Comments),
        rows = grepl("Warning", Comments))
    ) %>%
    gt::tab_style(
      style = list(
        cell_fill(color = "red" , alpha = 0.5)
      ),
      locations = cells_body(
        columns = vars(Comments),
        rows = grepl("Fail", Comments))
    )
}

# Data summary functions


#' Get median and range
#'
#' Gets median and range of numeric vector, rounding to specified digits with option
#' to show the number of missing values. Median will always be calculated using
#' non-missing values.
#'
#' @param values Numeric vector
#' @param digits_round Number of digits to round to. Default 1.
#' @param comma_separate Logical, default TRUE. Should large values be displayed with comma separators
#' @param add_missing Logical, default TRUE. Should the number of missing values be displayed in brackets
#' @param verbose Logical, default TRUE. Should a message be printed upon successful calculation, including the number of missing values.
#' @return String value of formatted median and range. 'median (min-max) [missing]'
#' @export
#' @examples
#' my_values <- c(1000:1010,NA)
#' get_median_range(my_values, digits_round = 1, comma_separate = TRUE, add_missing = TRUE, verbose = TRUE)
get_median_range <- function(values, digits_round = 1, comma_separate = TRUE, add_missing = TRUE, verbose = TRUE){
  assertthat::assert_that(mode(values) == "numeric" | all(is.na(values))) # can be any type of numeric, allow calculation if all missing
  assertthat::assert_that(length(values) > 0)

  fmt_num <- function(num){
    formatC(num, digits = digits_round, big.mark = ifelse(comma_separate,",",""), format = "f")
  }

  i_missing <- which(is.na(values))
  n_missing <- length(i_missing)
  has_missing <- n_missing > 0

  if(has_missing){
    values <- values[-i_missing]
  }

  med_string <- fmt_num(median(values))

  if(length(values) > 0){
    range_val <- fmt_num(range(values))
  } else {
    range_val <- c(NA,NA)
  }
  range_string <- paste(range_val, collapse = "-")
  med_range_string <- sprintf("%s (%s)", med_string, range_string)

  if(add_missing & has_missing){
    med_range_string <- sprintf("%s [%s]", med_range_string, n_missing)
  }

  if(verbose){
    cat(sprintf("Median and range calculated. Removed %s missing values", n_missing), sep = "\n")
  }

  return(med_range_string)
}

