read_h5_well_meta <- function (h5_file, target = "well") {
  assertthat::assert_that(is.character(h5_file))
  assertthat::assert_that(length(h5_file) == 1)
  target <- ifelse(grepl("^/", target), target, paste0("/",
                                                       target))
  h5_contents <- H5weaver::h5ls(h5_file)
  target_contents <- h5_contents[grepl(paste0("^", target,"$"),
                                       h5_contents$group), ]
  h5_meta_targets <- target_contents$full_name

  if (length(h5_meta_targets) > 0) {
    meta_list <- lapply(h5_meta_targets, function(h5_meta_target) {
      rhdf5::h5read(h5_file, h5_meta_target)
    })
    rhdf5::h5closeAll()
    names(meta_list) <- sub(".+/", "", h5_meta_targets)
    meta_list <- strip_1d_array_recursive(meta_list)
    meta_list <- convert_char_na_recursive(meta_list)
    df <- as.data.frame(meta_list, stringsAsFactors = FALSE)
    if("well_id" %in% names(df)){
      df <- df[, c("well_id", setdiff(names(df),"well_id"))]
    }
    df
  }
  else {
    stop("No well metadata found in h5_file.")
  }
}


read_hto_well_json <- function(fp){
  assertthat::assert_that(length(fp) == 1)

  json_list <- jsonlite::read_json(fp)

  well_id <- json_list$well_id
  sample_stats_list <- json_list$pbmc_sample_hto_stats
  sample_stats_df_list <- lapply(seq_along(sample_stats_list), function(i){
    df <- data.frame(pbmc_sample_id = names(sample_stats_list)[i])
    stats_list <- lapply(sample_stats_list[[i]], function(x){
      ifelse(is.null(x),NA, x)
    })
    df <- cbind(df, as.data.frame(stats_list))
    df
  })

  sample_stats_df <- do.call(rbind, sample_stats_df_list)
  results <- data.frame(well_id = rep(well_id, nrow (sample_stats_df)),sample_stats_df)
}
