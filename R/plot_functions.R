qc_stacked_barplot_mod <- function (meta, category_x = "batch_id", name_x = "Batch ID",
                                    category_y = "well_id", category_name = "Well ID", colorset_y = "varibow",
                                    name_y = "N Cells", as_fraction = FALSE, facet_formula = NULL, facet_rows = NULL,...) {
  assertthat::assert_that(sum(class(meta) %in% c("data.frame",
                                                 "data.table")) > 0)
  assertthat::assert_that(class(category_x) == "character")
  assertthat::assert_that(length(category_x) == 1)
  assertthat::assert_that(category_x %in% names(meta))
  assertthat::assert_that(class(name_x) == "character")
  assertthat::assert_that(length(name_x) == 1)
  assertthat::assert_that(class(category_y) == "character")
  assertthat::assert_that(length(category_y) == 1)
  assertthat::assert_that(category_y %in% names(meta))
  assertthat::assert_that(class(category_name) == "character")
  assertthat::assert_that(length(category_name) == 1)
  assertthat::assert_that(class(name_y) == "character")
  assertthat::assert_that(length(name_y) == 1)
  assertthat::assert_that(class(colorset_y) == "character")
  assertthat::assert_that(length(colorset_y) == 1)
  assertthat::assert_that(colorset_y %in% c("rainbow", "varibow"))
  assertthat::assert_that(class(as_fraction) == "logical")
  assertthat::assert_that(length(as_fraction) == 1)
  assertthat::assert_that(is.null(facet_formula) || rlang::is_formula(facet_formula))
  meta <- as.data.table(meta)

  if(!is.null(facet_formula)){
    formula_cols <- as.character(as.list(facet_formula))
    f_cols <- setdiff(formula_cols, "`~`")
    count_table <- meta[, .(n_cells = nrow(.SD)), by = mget(c(category_x,
                                                              category_y, f_cols))]
  } else {
    count_table <- meta[, .(n_cells = nrow(.SD)), by = mget(c(category_x,
                                                              category_y))]
  }
  plot_xpos <- data.frame(unique(count_table[[category_x]]))
  names(plot_xpos) <- category_x
  plot_xpos <- plot_xpos[order(plot_xpos[[category_x]]), ,
                         drop = FALSE]
  plot_xpos$xpos <- 1:nrow(plot_xpos)
  count_table <- count_table[plot_xpos, on = category_x]
  plot_fills <- data.frame(unique(count_table[[category_y]]))
  names(plot_fills) <- category_y
  if (colorset_y == "rainbow") {
    set.seed(3030)
    plot_fills$fill <- sample(grDevices::rainbow(nrow(plot_fills)),
                              nrow(plot_fills))
  }
  else if (colorset_y == "varibow") {
    set.seed(3030)
    plot_fills$fill <- sample(immutils::varibow(nrow(plot_fills)),
                              nrow(plot_fills))
  }
  plot_fills <- plot_fills[order(plot_fills[[category_y]]),
  ]
  count_table <- count_table[plot_fills, on = category_y]
  count_table <- count_table[order(get(category_y), decreasing = TRUE)]
  if (as_fraction) {
    count_table <- count_table[, `:=`(ymax, cumsum(n_cells)/sum(n_cells)),
                               by = list(get(category_x))]
    count_table <- count_table[, `:=`(ymin, shift(ymax,
                                                  fill = 0, type = "lag")), by = list(get(category_x))]
  }
  p <- ggplot2::ggplot() + ggplot2::geom_rect(data = count_table,
                                              ggplot2::aes(xmin = xpos - 0.4, xmax = xpos + 0.4, ymin = ymin,
                                                           ymax = ymax, fill = fill)) + ggplot2::scale_fill_identity(category_name,
                                                                                                                     breaks = plot_fills$fill, labels = plot_fills[[category_y]],
                                                                                                                     guide = "legend") + ggplot2::scale_x_continuous(name_x,
                                                                                                                                                                     breaks = plot_xpos$xpos, labels = plot_xpos[[category_x]]) +
    ggplot2::scale_y_continuous(name_y) + ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 1, vjust = 0.3))
  if(!is.null(facet_formula)){
    p <- p +
      facet_wrap(facet_formula, nrow = facet_rows, ...)
  }
  p
}

qc_aligned_barplot_mod <- function (meta, category_x = "batch_id",
                                    name_x = "Batch ID",
                                    category_y = "well_id",
                                    category_name = "Well ID",
                                    colorset_y = "varibow",
                                    name_y = "N Cells", padding = 0.2,
                                    facet_formula = NULL, facet_rows = NULL,...) {
  assertthat::assert_that(sum(class(meta) %in% c("data.frame",
                                                 "data.table")) > 0)
  assertthat::assert_that(class(category_x) == "character")
  assertthat::assert_that(length(category_x) == 1)
  assertthat::assert_that(category_x %in% names(meta))
  assertthat::assert_that(class(name_x) == "character")
  assertthat::assert_that(length(name_x) == 1)
  assertthat::assert_that(class(category_y) == "character")
  assertthat::assert_that(length(category_y) == 1)
  assertthat::assert_that(category_y %in% names(meta))
  assertthat::assert_that(class(category_name) == "character")
  assertthat::assert_that(length(category_name) == 1)
  assertthat::assert_that(class(name_y) == "character")
  assertthat::assert_that(length(name_y) == 1)
  assertthat::assert_that(class(colorset_y) == "character")
  assertthat::assert_that(length(colorset_y) == 1)
  assertthat::assert_that(colorset_y %in% c("rainbow", "varibow"))
  assertthat::assert_that(class(padding) == "numeric")
  assertthat::assert_that(length(padding) == 1)
  assertthat::assert_that(padding < 1)
  tidy_x <- rlang::parse_expr(category_x)
  tidy_y <- rlang::parse_expr(category_y)
  meta <- as.data.table(meta)
  if(!is.null(facet_formula)){
    formula_cols <- as.character(as.list(facet_formula))
    f_cols <- setdiff(formula_cols, "`~`")
    count_table <- meta[, .(n_cells = nrow(.SD)), by = mget(c(category_x,
                                                              category_y, f_cols))]
  } else {
    count_table <- meta[, .(n_cells = nrow(.SD)), by = mget(c(category_x,
                                                              category_y))]
  }
  plot_xpos <- data.frame(unique(count_table[[category_x]]))
  names(plot_xpos) <- category_x
  plot_xpos <- plot_xpos[order(plot_xpos[[category_x]]), ,
                         drop = FALSE]
  plot_xpos$xpos <- 1:nrow(plot_xpos)
  count_table <- count_table[plot_xpos, on = category_x]

  plot_fills <- data.frame(unique(count_table[[category_y]]))
  names(plot_fills) <- category_y
  if (colorset_y == "rainbow") {
    set.seed(3030)
    plot_fills$fill <- sample(grDevices::rainbow(nrow(plot_fills)),
                              nrow(plot_fills))
  }
  else if (colorset_y == "varibow") {
    set.seed(3030)
    plot_fills$fill <- sample(immutils::varibow(nrow(plot_fills)),
                              nrow(plot_fills))
  }
  plot_fills <- plot_fills[order(plot_fills[[category_y]]),
  ]
  count_table <- count_table[plot_fills, on = category_y]

  group_maxes <- count_table[, .(group_max = max(n_cells)),
                             by = list(get(category_y))]
  names(group_maxes)[1] <- category_y
  group_maxes <- group_maxes[order(get(category_y), decreasing = TRUE)]
  group_maxes <- group_maxes[, `:=`(cum_max, cumsum(group_max))]
  group_maxes <- group_maxes[, `:=`(group_center, cum_max -
                                      group_max/2)]
  group_maxes <- group_maxes[, `:=`(padded_center, group_center +
                                      (max(cum_max) * (padding/nrow(group_maxes))) * (1:nrow(group_maxes) -
                                                                                        1))]
  group_maxes <- group_maxes[, `:=`(padded_base, padded_center -
                                      group_max/2)]
  group_maxes <- group_maxes[, `:=`(padded_top, padded_center +
                                      group_max/2)]
  count_table <- count_table[group_maxes, on = category_y]
  count_table <- count_table[order(get(category_y), decreasing = TRUE)]
  count_table <- count_table[, `:=`(ymax, cumsum(n_cells)),
                             by = list(get(category_x))]
  count_table <- count_table[, `:=`(ymin, shift(ymax, fill = 0,
                                                type = "lag")), by = list(get(category_x))]
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = count_table, ggplot2::aes(xmin = xpos - 0.4,
                                                        xmax = xpos + 0.4,
                                                        ymin = padded_base,
                                                        ymax = padded_base + n_cells,
                                                        fill = fill)) +
    ggplot2::geom_hline(data = count_table, ggplot2::aes(yintercept = padded_base)) +
    ggplot2::geom_hline(data = count_table, ggplot2::aes(yintercept = padded_top), linetype = "dashed") +
    ggplot2::scale_fill_identity(category_name, breaks = plot_fills$fill,
                                 labels = plot_fills[[category_y]], guide = "legend") +
    ggplot2::scale_x_continuous(name_x, breaks = plot_xpos$xpos,
                                labels = plot_xpos[[category_x]]) +
    ggplot2::scale_y_continuous(name_y, breaks = c(group_maxes$padded_base, group_maxes$padded_top),
                                labels = c(rep("", nrow(group_maxes)), group_maxes$group_max),
                                expand = ggplot2::expansion(c(0, 0.02))) + ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.3))

  if(!is.null(facet_formula)){
    p <- p +
      facet_wrap(facet_formula, nrow = facet_rows, ...)
  }
  p
}




#' Add Axis Spacing for Faceted Plotly
#'
#'
#'
#' @param plotly_obj An object generated by ggplotly
#' @param axis String value of "x" or "y". Which axis title to add spacing to.
#' If need to add spacing to both, make separate calls to `add_axis_title_spacing_plotly()`
#' @param n_lines Numeric value. Number of lines worth of spacing to add.
#' @return A the original plotly object with updated axis name
#' @examples
#' library(ggplot2)
#' library(plotly)
#' set.seed(1)
#' x <- sapply(1:10,function(x){paste(LETTERS[sample(1:26,10,replace = T)],collapse="")})
#' y <- sapply(1:20,function(x){paste(LETTERS[sample(1:26,10,replace = T)],collapse="")})
#' z <- rep(c("A","B"), times = 10)
#' df <- data.frame(x, y, z)
#' g <- ggplot(df, aes(x, y))+
#'   geom_point()+
#'   facet_wrap(~z, nrow =1) +
#'   xlab("X TITLE") +
#'   ylab("Y TITLE") +
#'   theme(axis.text.x = element_text(angle = 90))
#' g
#' gp <- ggplotly(g)
#' gp
#' gp %>%
#'   add_axis_title_spacing_plotly("x", 3) %>%
#'   add_axis_title_spacing_plotly("y", 3)
#'
add_axis_title_spacing_plotly <- function(plotly_obj, axis, n_lines){
  assertthat::assert_that(is.character(axis))
  axis <- tolower(axis)
  assertthat::assert_that(axis %in% c("x","y"))
  assertthat::assert_that(is.numeric(n_lines))

  axis_string <- paste0(axis,"axis")

  annotation_index <- (axis=="y") + 1
  orig_title <- plotly_obj$x$layout$annotations[[annotation_index]]$text
  if(is.null(orig_title)){
    stop(sprintf("No title detected in plotly object for axis %s",x))
  }

  spacing_string <- paste("",rep("\n",n_lines), collapse = "")
  if(axis == "x"){
    newtitle <- paste0(spacing_string, orig_title)
  } else if (axis == "y"){
    newtitle <- paste0(orig_title, spacing_string)
  }

  plotly_obj$x$layout$annotations[[annotation_index]]$text <- newtitle

  return(plotly_obj)
}

#' Add Axis Spacing for Faceted Plotly
#'
#'
#'
#' @param plotly_obj An object generated by ggplotly
#' @param axis String value of "x" or "y". Which axis title to add spacing to.
#' If need to add spacing to both, make separate calls to `add_axis_title_spacing_plotly()`
#' @param adjustment Numeric value. Vertical (for x-axis) or horizontal (for y-axis) adjustment for axis title
#' @return A the original plotly object with updated axis name
#' @examples
#' library(ggplot2)
#' library(plotly)
#' set.seed(1)
#' x <- sapply(1:10,function(x){paste(LETTERS[sample(1:26,10,replace = T)],collapse="")})
#' y <- sapply(1:20,function(x){paste(LETTERS[sample(1:26,10,replace = T)],collapse="")})
#' z <- rep(c("A","B"), times = 10)
#' df <- data.frame(x, y, z)
#' g <- ggplot(df, aes(x, y))+
#'   geom_point()+
#'   facet_wrap(~z, nrow =1) +
#'   xlab("X TITLE") +
#'   ylab("Y TITLE") +
#'   theme(axis.text.x = element_text(angle = 90))
#' g
#' gp <- ggplotly(g)
#' gp
#' gp %>%
#'   add_axis_title_spacing_plotly("x", 3) %>%
#'   add_axis_title_spacing_plotly("y", 3)
#'
adjust_axis_title_spacing_plotly <- function(plotly_obj, axis, adjustment){
  assertthat::assert_that(is.character(axis))
  axis <- tolower(axis)
  assertthat::assert_that(axis %in% c("x","y"))
  assertthat::assert_that(is.numeric(adjustment))

  annotation_index <- (axis=="y") + 1

  if(axis == "x"){
    plotly_obj$x$layout$annotations[[annotation_index]]$y <- adjustment
  } else if (axis == "y"){
    plotly_obj$x$layout$annotations[[annotation_index]]$x <- adjustment
  }

  return(plotly_obj)
}

#' 
plot_umap_report <- function(df, x_col, x_lab, y_col, y_lab, title, point_size, color_col, scale_color_fun,...){
  g <- ggplot(df, aes_string(x_col, y_col)) + 
    geom_point(alpha = 1, size = point_size, aes_string(color = color_col)) +
    ggtitle(title)+
    xlab(x_lab) +
    ylab(y_lab) +
    scale_color_fun() +
    theme_bw() +
    theme(aspect.ratio = 1/1,
          text = element_text(size = 20))
  return(g)
  
} 

scale_color_genes <- function(max_genes){
  function(...){
    scale_color_gradientn(limits = c(0, max_genes),
                          colours = c("blue","deepskyblue","green3", "yellow","orange","red","darkred"),
                          values = scales::rescale(c(0,500, 1000, 2000, 3000, 4000, max_genes),
                                                   from = c(0, max_genes)),
                          breaks = c(0,2000,4000,6000,8000), ...)
  }
}

scale_color_umis <- function(max_umi){
  function(...){
    scale_color_gradientn(limits = c(0, max_umi),
                          colours = c("blue","deepskyblue","green3", "yellow","orange","red","darkred"),
                          values = scales::rescale(c(0,1000, 3000, 5000, 7500, 10000, max_umi),
                                                   from = c(0, max_umi)), ...)
  }
}

scale_color_fct_mito <- function(...){
  scale_color_gradientn(limits = c(0,1),
                       colors = c("blue", "green3","yellow","red"), 
                       breaks = c(0,0.25,0.5,0.75,1),...)
  
  
}