library(optparse)

option_list <- list(
  make_option(opt_str = c("-b","--batch_id"),
              type = "character",
              default = NULL,
              help = "Batch identifier",
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
  make_option(opt_str = c("-n","--n_cores"),
              type = "integer",
              default = NULL,
              help = "Number of cores for multicore processing",
              metavar = "integer"),
  make_option(opt_str = c("-m","--mc_mb_limit"),
              type = "integer",
              default = NULL,
              help = "Maximum size in Mb allowed for exporting globals to each worker in multicore processing",
              metavar = "integer"),
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
                             "_scrna_batch_summary_parent.Rmd"))

file.copy(system.file("rmarkdown/scrna_batch_summary_parent.Rmd", package = "batchreporter"),
          rmd_path,
          overwrite = TRUE)

rmarkdown::render(
  input = rmd_path,
  params = list(batch   = args$batch_id,
                in_dir  = args$in_dir,
                in_key  = args$in_key,
                in_batch_meta = args$in_batch_meta,
                n_cores = args$ncores,
                mc_mb_limit = args$mc_mb_limit,
                out_dir = args$out_dir),
  output_file = args$out_html,
  quiet = TRUE
)

file.remove(rmd_path)

# Remove figure files generated
# fig_path <- file.path(args$out_dir,"figures")
# file.remove(fig_path, recursive = TRUE)
