# Set Up
remove(list=ls())
library(stringr)
library(H5weaver)
library(doMC)

# Make example files by reducing all data files to 1000 genes to speed up computation
# time in development.
# Based on B002 data

# Source data: gs:/private-analysis-store/B002-files
# Saved all files to local directory

source_data_dir <-"~/rnaseq_qc_report/test_data/B002-hise/B002-files"
mydir <- "~/packages/scrna-batch-report/"
outdir <- "~/packages/scrna-batch-report/inst"

# Modify: B002 files
#         Deidentify batch and sample names
# Batch: T001
# Samples: S01-S26

all_labeled_h5 <- dir(source_data_dir, 
                      pattern = ".*_labeled.h5$",
                      full.names = TRUE, 
                      recursive = TRUE)
all_labeled_h5
# [1] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_IMM19_698_labeled.h5"           
# [2] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00021-01_labeled.h5"          
# [3] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00022-01_labeled.h5"          
# [4] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00024-01_labeled.h5"          
# [5] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00053-01_labeled.h5"          
# [6] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00055-01_labeled.h5"          
# [7] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00056-01_labeled.h5"          
# [8] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00057-01_labeled.h5"          
# [9] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00059-01_labeled.h5"          
# [10] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00060-01_labeled.h5"          
# [11] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00061-01_labeled.h5"          
# [12] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/labeled/B002-P1_PB00062-01_labeled.h5"          
# [13] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_IMM19_698_labeled.h5" 
# [14] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00004-01_labeled.h5"
# [15] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00006-01_labeled.h5"
# [16] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00010-01_labeled.h5"
# [17] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00011-01_labeled.h5"
# [18] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00012-01_labeled.h5"
# [19] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00013-01_labeled.h5"
# [20] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00014-01_labeled.h5"
# [21] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00015-01_labeled.h5"
# [22] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00016-01_labeled.h5"
# [23] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00017-01_labeled.h5"
# [24] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/labeled/B002-P2_PB00018-01_labeled.h5"

all_json <- dir(source_data_dir, 
                pattern = "hto_processing_metrics.json",
                full.names = TRUE, 
                recursive = TRUE)
all_json
# [1] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C1W1_hto_processing_metrics.json"
# [2] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C1W2_hto_processing_metrics.json"
# [3] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C1W3_hto_processing_metrics.json"
# [4] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C1W4_hto_processing_metrics.json"
# [5] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C1W5_hto_processing_metrics.json"
# [6] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C1W6_hto_processing_metrics.json"
# [7] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C1W7_hto_processing_metrics.json"
# [8] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C1W8_hto_processing_metrics.json"
# [9] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C2W1_hto_processing_metrics.json"
# [10] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C2W2_hto_processing_metrics.json"
# [11] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C2W3_hto_processing_metrics.json"
# [12] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1C2W4_hto_processing_metrics.json"
# [13] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C2W5_hto_processing_metrics.json"
# [14] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C2W6_hto_processing_metrics.json"
# [15] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C2W7_hto_processing_metrics.json"
# [16] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C2W8_hto_processing_metrics.json"
# [17] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C3W1_hto_processing_metrics.json"
# [18] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C3W2_hto_processing_metrics.json"
# [19] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C3W3_hto_processing_metrics.json"
# [20] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C3W4_hto_processing_metrics.json"
# [21] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C3W5_hto_processing_metrics.json"
# [22] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C3W6_hto_processing_metrics.json"
# [23] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C3W7_hto_processing_metrics.json"
# [24] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P2C3W8_hto_processing_metrics.json"

all_multiplet <- dir(source_data_dir,
                     pattern = ".*multiplet.h5",
                     full.names = TRUE, 
                     recursive = TRUE)
all_multiplet <- all_multiplet[!grepl("transposed", all_multiplet)]
all_multiplet
# [1] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/B002-P1_multiplet.h5"          
# [2] "/home/lauren_okada/rnaseq_qc_report/test_data/B002-hise/B002-files/merged_h5/B002-P2_multiplet.h5"

all_control <- dir("~/rnaseq_qc_report/test_data/controls",
                   pattern = ".*labeled.h5",
                   full.names = TRUE, 
                   recursive = TRUE)
all_control
# [1] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B001-P1_IMM19_709_labeled.h5"                               
# [2] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B002-P1_IMM19_698_labeled.h5"                               
# [3] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B002-P2_IMM19_698_labeled.h5"                               
# [4] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B004-P1_IMM19-695_labeled.h5"                               
# [5] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B004-P2_IMM19-695_labeled.h5"                               
# [6] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B010-P1_IMM19_394_2020-10-07T03:44:24.396314868Z_labeled.h5"
# [7] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B026-P1_IMM19_405_2020-10-10T20:09:23.161979427Z_labeled.h5"
# [8] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B026-P2_IMM19_405_2020-10-10T20:59:49.850564489Z_labeled.h5"
# [9] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B027-P1_IMM19_406_2020-09-29T00:27:07.534474463Z_labeled.h5"
# [10] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B027-P2_IMM19_406_2020-10-06T16:42:55.19194787Z_labeled.h5" 
# [11] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B033-P1_IMM19_408_2020-10-16T15:34:33.152518772Z_labeled.h5"
# [12] "/home/lauren_okada/rnaseq_qc_report/test_data/controls/B033-P2_IMM19_408_2020-10-17T18:48:16.001839234Z_labeled.h5"

# De-identification IDs
batch <- "T001"  
samples <- paste0("S",stringr::str_pad(1:26, width = 2, pad = "0"))
samples[c(1,13)] <- c("IMM00-002","IMM00-002")
old_samples <- gsub(".*B002-P[12]_","",gsub("_labeled.h5","",all_labeled_h5))
old_samples <- c(old_samples,"PB00019-01","PB00020-01")
old_control_names <- gsub(".*B0\\d+-P[12]_","",gsub("_labeled.h5","",all_control))
control_names <- paste0("IMM00-", stringr::str_pad(c(1,2,2,3,3,4,5,5,6,6,7,7), width = 3, pad = "0"))
control_batch <- paste0("T", stringr::str_pad(c(0,1,1,2,2,3,4,4,5,5,6,6), width = 3, pad = "0"))
old_control_batch <- gsub("-P[12].*","",gsub("_labeled.h5","",basename(all_control)))

# Genes to keep
chrM_genes <- fread(system.file("reference/GRCh38_10x_chrM_gene_metadata.csv.gz",
                                package = "H5weaver"))

genes <- c("CD3E", "CD4", "CD8A", "HLA-DRA", "CD14", "FCGR3A", "IL3RA", "ITGAX", 
           "CD19", "MS4A1", "CD79A", "NCAM1", "NKG7", "PPBP", "IL7R")

keep_genes <- c(chrM_genes$name, genes)
n_random_genes <- 1000-length(keep_genes)

#===============================================#
# Make output folders
#===============================================#
if(!dir.exists(file.path(outdir,"extdata","labeled_h5"))){
  dir.create(file.path(outdir,"extdata","labeled_h5"))
}

if(!dir.exists(file.path(outdir,"extdata","multiplet_h5"))){
  dir.create(file.path(outdir,"extdata","multiplet_h5"))
}

if(!dir.exists(file.path(outdir,"extdata","control"))){
  dir.create(file.path(outdir,"extdata","control"))
}

if(!dir.exists(file.path(outdir,"extdata","hash"))){
  dir.create(file.path(outdir,"extdata","hash"))
}

doMC::registerDoMC(cores = 8)
#===============================================#
# Modify labeled h5 by subsetting to 1000 genes
#===============================================#
# define gene subset, keeping all mt genes and genes of interest 
sample1_list <- H5weaver::h5dump(all_labeled_h5[1])
sample1_list <- h5_list_convert_to_dgCMatrix(sample1_list)

set.seed(20201120)
other_genes <- setdiff(sample1_list$matrix$features$name, keep_genes)
gene_1k <- c(sample(other_genes, n_random_genes), keep_genes)

# Modify each labeled h5 file
foreach(i=seq_along(all_labeled_h5)) %dopar% {
# for (i in 18:length(all_labeled_h5)){
  sample_list <- h5dump(all_labeled_h5[[i]])
  sample_list <- h5_list_convert_to_dgCMatrix(sample_list)
  
  # identify index of selected genes
  i_gene_keep <- match(gene_1k, sample_list$matrix$features$name)
  
  # keep the selected genes in the count matrix
  sample_list$matrix_dgCMatrix <- sample_list$matrix_dgCMatrix[i_gene_keep,]
  
  # keep only the selected genes as "features" in the h5
  sample_list$matrix$features$name <- sample_list$matrix$features$name[i_gene_keep]
  sample_list$matrix$features$genome <- sample_list$matrix$features$genome[i_gene_keep]
  sample_list$matrix$features$feature_type <- sample_list$matrix$features$feature_type[i_gene_keep]
  
  # modify metadata to generic names (batch, ) and correct format
  sample_list$matrix$observations$well_id <- gsub("-RNA","",gsub("B002", batch, sample_list$matrix$observations$well_id))
  sample_list$matrix$observations$chip_id <- gsub("W.*","",sample_list$matrix$observations$well_id)
  sample_list$matrix$observations$pool_id <- gsub("C.*","",sample_list$matrix$observations$chip_id)
  sample_list$matrix$observations$batch_id <- batch
  sample_list$matrix$observations$pbmc_sample_id <- samples[i]
  
  # return to original format and save new file
  sample_list <- h5_list_convert_from_dgCMatrix(sample_list)
  pool <- unique(sample_list$matrix$observations$pool_id)
  write_h5_list(sample_list,
                file.path(mydir, sprintf("inst/extdata/labeled_h5/%s_%s_labeled.h5", pool, samples[i])),
                overwrite = TRUE)
  h5closeAll()
}

#===============================================#
# Modify Multiplets to same 1000 genes
#===============================================#
# for (i in seq_along(all_multiplet)){
foreach(i=seq_along(all_multiplet)) %dopar% {
  multi_list <- h5dump(all_multiplet[i])
  multi_list <- h5_list_convert_to_dgCMatrix(multi_list)
  
  # identify index of selected genes
  i_gene_keep <- match(gene_1k, multi_list$matrix$features$name)
  
  # keep the selected genes in the count matrix
  multi_list$matrix_dgCMatrix <- multi_list$matrix_dgCMatrix[i_gene_keep,]
  
  # keep only the selected genes as "features" in the h5
  multi_list$matrix$features$name <- multi_list$matrix$features$name[i_gene_keep]
  multi_list$matrix$features$genome <- multi_list$matrix$features$genome[i_gene_keep]
  multi_list$matrix$features$feature_type <- multi_list$matrix$features$feature_type[i_gene_keep]
  
  # modify metadata to generic names (batch, ) and correct format
  multi_list$matrix$observations$well_id <- gsub("-RNA","",gsub("B002", batch, multi_list$matrix$observations$well_id))
  multi_list$matrix$observations$chip_id <- gsub("W.*","",multi_list$matrix$observations$well_id)
  multi_list$matrix$observations$pool_id <- gsub("C.*","",multi_list$matrix$observations$chip_id)
  multi_list$matrix$observations$batch_id <- batch
  
  # modify samples
  multi_sample_list <- strsplit(multi_list$matrix$observations$pbmc_sample_id, split = ";" )
  multi_sample_list_new <- lapply(multi_sample_list, function(x){
    newnames <- samples[match(x, old_samples)]
  })
  multi_samples_new <- sapply(multi_sample_list_new, function(x){
    paste(x, collapse = ";")
  })
  multi_list$matrix$observations$pbmc_sample_id <- multi_samples_new
  
  # return to original format and save new file
  multi_list <- h5_list_convert_from_dgCMatrix(multi_list)
  pool <- unique(multi_list$matrix$observations$pool_id)
  write_h5_list(multi_list,
                file.path(mydir, sprintf("inst/extdata/multiplet_h5/%s_multiplet.h5", pool)),
                overwrite = TRUE)
  h5closeAll()
}

#===============================================#
# Modify Control h5 to same 1000 genes
#===============================================#

foreach (i= seq_along(all_control)) %dopar% {
  control_list <- h5dump(all_control[i])
  control_list <- h5_list_convert_to_dgCMatrix(control_list)
  
  # identify index of selected genes
  i_gene_keep <- match(gene_1k, control_list$matrix$features$name)
  
  # keep the selected genes in the count matrix
  control_list$matrix_dgCMatrix <- control_list$matrix_dgCMatrix[i_gene_keep,]
  
  # keep only the selected genes as "features" in the h5
  control_list$matrix$features$name <- control_list$matrix$features$name[i_gene_keep]
  control_list$matrix$features$genome <- control_list$matrix$features$genome[i_gene_keep]
  control_list$matrix$features$feature_type <- control_list$matrix$features$feature_type[i_gene_keep]
  
  # modify metadata to generic names (batch, ) and correct format
  control_list$matrix$observations$well_id <- gsub("-RNA","",gsub(old_control_batch[i], control_batch[i], control_list$matrix$observations$well_id))
  control_list$matrix$observations$chip_id <- gsub("W.*","",control_list$matrix$observations$well_id)
  control_list$matrix$observations$pool_id <- gsub("C.*","",control_list$matrix$observations$chip_id)
  control_list$matrix$observations$batch_id <- control_batch[i]
  control_list$matrix$observations$pbmc_sample_id <- control_names[i]
  
  # return to original format and save new file
  control_list <- h5_list_convert_from_dgCMatrix(control_list)
  pool <- unique(control_list$matrix$observations$pool_id)
  write_h5_list(control_list,
                file.path(mydir, sprintf("inst/extdata/control/%s_%s_labeled.h5", pool, control_names[i])),
                overwrite = TRUE)
  h5closeAll()
}


#===============================================#
# Modify names in json files
#===============================================#
for (i in seq_along(all_json)){
  json_list <- jsonlite::read_json(all_json[i])
  
  json_list$well_id <- gsub("B002", batch, json_list$well_id)
  
  # substitute all old sample names with new, except if NA
  iNA <- grep("^NA_", names(json_list$pbmc_sample_hto_stats))
  names(json_list$pbmc_sample_hto_stats)[-iNA] <- samples[match(names(json_list$pbmc_sample_hto_stats)[-iNA], old_samples)]
  
  qc_list_json <- jsonlite::toJSON(json_list,
                                   auto_unbox = TRUE,
                                   pretty = TRUE,
                                   null = "null")
  writeLines(qc_list_json,
             file.path(mydir, sprintf("inst/extdata/hash/%s_hto_processing_metrics.json",  json_list$well_id)))
  
}

