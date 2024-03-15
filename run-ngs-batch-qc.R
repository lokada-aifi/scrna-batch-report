library(optparse)

option_list <- list(
  make_option(opt_str = c("-b","--batch_id"),
              type = "character",
              default = NULL,
              help = "Batch identifier",
              metavar = "character"),
  make_option(opt_str = c("-m","--in_method"),
              type = "character",
              default = NULL,
              help = "Input batch pipeline modality string",
              metavar = "character"),
  make_option(opt_str = c("-i","--in_dir"),
              type = "character",
              default = NULL,
              help = "Input directory containing h5 and json files",
              metavar = "character"),
  make_option(opt_str = c("-k","--in_key"),
              type = "character",
              default = NULL,
              help = "Input sample sheet",
              metavar = "character"),
  make_option(opt_str = c("-t","--in_batch_meta"),
              type = "character",
              default = NULL,
              help = "Input batch metadata json",
              metavar = "character"),
  make_option(opt_str = c("-c","--in_config"),
              type = "character",
              default = NULL,
              help = "Input analysis config csv",
              metavar = "character"),
  make_option(opt_str = c("-d","--out_dir"),
              type = "character",
              default = NULL,
              help = "Output Directory",
              metavar = "character"),
  make_option(opt_str = c("-o","--out_html"),
              type = "character",
              default = NULL,
              help = "Output HTML run summary file",
              metavar = "character")
)

opt_parser <- OptionParser(option_list = option_list)

args <- parse_args(opt_parser)

if(is.null(args$batch_id)) {
  print_help(opt_parser)
  stop("No parameters supplied.")
}

if(!dir.exists(args$out_dir)) {
  dir.create(args$out_dir)
}

rmd_path <- file.path(args$out_dir,
                      paste0(args$batch_id,
                             "_ngs_batch_qc_report.Rmd"))

message(paste("Generating ngs_batch_qc_report.Rmd into", rmd_path))
message(paste("Current working directory", getwd()))

file.copy("batchreporter/inst/rmarkdown/ngs_batch_qc_report.Rmd", rmd_path,
          overwrite = TRUE)

rmarkdown::render(
  input = rmd_path,
  params = list(batch_id = args$batch_id,
                in_method = args$in_method,
                in_dir  = args$in_dir,
                in_key  = args$in_key,
                in_batch_meta = args$in_batch_meta,
                in_config = args$in_config,
                out_dir = args$out_dir),
  output_file = args$out_html,
  quiet = TRUE
)

file.remove(rmd_path)