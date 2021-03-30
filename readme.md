# batchreporter

R functions and scripts for summarizing and performing batch-level QC on AIFI sequencing pipeline data.  

<a id="contents"></a>

## Contents

#### [Dependencies](#dependencies)

#### [Installation](#installation)

#### [Available Reports](#available_report)

#### [Running Reports](#batch_report)

#### [scRNA Cell Hashing Batch Report](#scrna_batch_report)
- [Parameters](#scrna_report_param)
- [Sample Sheet Guidelines](#scrna_sample_sheet)
- [Outputs](#scrna_report_out)
- [Tests](#scrna_report_test)

<a id="dependencies"></a>

## Dependencies    

This repository requires that `pandoc` and `libhdf5-devel` libraries are installed as dependencies of the `H5weaver` functions:
```
sudo apt-get install pandoc libhdf5-devel
```

It also depends on the internal AIFI `H5weaver` and `HTOparser` packages. Additional CRAN packages needed to run reports can be found in the `Description` file.

CRAN packages can be installed in R using:
```
# Example
install.packages("jsonlite")
install.packages("rmarkdown")
install.packages("optparse")
```

`HTOparser` and `H5weaver` are internally developed packages found in the aifimmunology Github repositories. Because it is a private repository, you may need to provided a [Github Personal Access Token](https://github.com/settings/tokens) for installation:
```
Sys.setenv(GITHUB_PAT = "[your_PAT_here]")
devtools::install_github("aifimmunology/HTOparser")
devtools::install_github("aifimmunology/H5weaver")
```

[Return to Contents](#contents)

<a id="installation"></a>

## Installation
`batchreporter` is an R package with associated executable scripts. First install dependencies. Then install the R package from the AIFImmunology Github repository:

```
Sys.setenv(GITHUB_PAT = "[your_PAT_here]")
devtools::install_github("aifimmunology/batchreporter")
```  
To run scripts, clone the GitHub repository and run the desired wrapper script within the local clone.

[Return to Contents](#contents)  

<a id="available_report"></a>

## Available Reports and Scripts 
Available batch reports are as follows:
- [scRNA cell hashing batch report](#scrna_batch_report): Batch report for stand alone scRNA Cell-Hashing + labeling pipelines, run post-labeling

[Return to Contents](#contents)  

<a id="batch_report"></a>

## Running Batch Reports  
Batch reports are run by executing an R wrapper script with the appropriate options. Options may be unique to different report types. The wrapper script will feed
supplied parameters to the Rmarkdown document to render a new report, output to locations specified by arguments.  
 
[Return to Contents](#contents)  

<a id="scrna_batch_report"></a>

## scRNA Cell Hashing Batch Report

The `run-scrna-batch-qc.R` wrapper script renders `scrna-batch-report-parent.Rmd` and is compatible with results files generated by [`cell-hashing-pipeline`](https://github.com/aifimmunology/cell-hashing-pipeline) and [`cell-labeling-pipeline`](https://github.com/aifimmunology/cell-labeling-pipeline). Output files from both processes are required for processing. This top level "parent" document will utilize "child" rmarkdown reports that reside within `batchreporter`  

[Return to Contents](#contents)  

<a id="scrna_report_param"></a>

#### Input Parameters

There are 9 parameters for this script:  

* `-b or --batch_id`:  The batch name, ie B001
* `-i or --in_dir`: The input directory containing the files to process. Should include the following subdirectories:  
  * `labeled_h5`: Contains all sample labeled.h5 files for the batch generated by [`cell-labeling-pipeline`](https://github.com/aifimmunology/cell-labeling-pipeline)  
  * `multiplet`: Contains all pool multiplet.h5 files for the batch generated by `run_h5_merge_by_hash.R` from [`cell-hashing-pipeline`](https://github.com/aifimmunology/cell-hashing-pipeline)  
  * `hash`: Contains all well hto_processing_metrics .json files for the batch generated by `run_hto_processing.R` from [`cell-hashing-pipeline`](https://github.com/aifimmunology/cell-hashing-pipeline)  
  * `control`: Contains all batch control labeled.h5 files generated by [`cell-labeling-pipeline`](https://github.com/aifimmunology/cell-labeling-pipeline) for a standing set of reference batches. Included reference batches may be added to over time, but initially include B004, B007, and B025.
* `-k or --in_key`: A 6-column .csv Sample Sheet (see format [below](#scrna_sample_sheet)) of identifiers for all samples in the batch.
* `-c or --in_config`: A .csv file of analysis parameters. For standard pbmc processing with Seurat 3 labeling may use the default config. If not supplied, will default to the standing example file within `batchreporter` ('default_rna_config_v1.csv')
* `-t or --in_batch_meta`: A .json file of key value pairs describing relevant reference datasets and software versions used in the analysis pipeline processing of the batch. If provided, all values will be echoed as a flat table on report. 
* `-n or --n_cores`: An integer value of number of cores to use for multithreaded processes, used by Seurat functions  
* `-m or --mc_mb_limit`: An integer value of number of maximum size in Mb allowed for exporting globals to each worker in multicore processing (for futures R package). Defaults to 20000.  
* `-d or --out_dir`: A directory path to use to output the HTO Processing results  
* `-o or --out_html`: A filename to use to output the HTML summary report file  

Test runs may be performed by supplying only the -b and -o arguments, as seen in test [below](#batch_report_test). 

[Return to Contents](#contents)  

<a id="scrna_sample_sheet"></a>

##### Sample Sheet Format

`run-scrna-batch-qc.R` requires a **Sample Sheet**, provided as the -k parameter, which has 6 comma-separated columns: SampleID, Type, BatchID, HashTag, PoolID, and WellID. `Type` is either `Sample` for normal samples or `Control` for bridging controls. 
All wells associated with each sample should be collapsed into a ";"-delimited string. Expected formatting for variables can be seen below. 

For example:
```
SampleID,Type,BatchID,HashTag,PoolID,WellID
PB5206W2,Sample,B001,HT1,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB5206W3,Sample,B001,HT2,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB5206W4,Sample,B001,HT3,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB5206W5,Sample,B001,HT4,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB5206W6,Sample,B001,HT5,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB5206W7,Sample,B001,HT6,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB7626W2,Sample,B001,HT7,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB7626W3,Sample,B001,HT8,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB7626W4,Sample,B001,HT9,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB7626W5,Sample,B001,HT10,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB7626W6,Sample,B001,HT12,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
PB7626W7,Sample,B001,HT13,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
IMM19-711,Control,B001,HT14,B001-P1,C1W1;C1W2;C1W3;C1W4;C1W5;C1W6;C1W7;C1W8;C2W1;C2W2;C2W3;C2W4;C2W5
```

Sample sheets should be provided for each dataset. When running script without a **Sample Sheet**, for testing purposes, the -k parameter is omitted and the script uses an example file provided in `batchreporter`.  

An example run:
```
git clone https://github.com/aifimmunology/scrna-batch-qc.git

Rscript --vanilla \
    /home/jupyter/scrna-batch-report/run-scrna-batch-qc.R \
    -b B026  \
    -i /home/jupyter/test_data/B026 \
    -k /home/jupyter/test_data/B026/sample_key_B026.csv   \
    -c /home/jupyter/local.lib/batchreporter/extdata/default_rna_config_v1.csv  \
    -d /home/jupyter/test_output/ \
    -o  B026_20210317.html \
    -t  /home/jupyter/local.lib/batchreporter/extdata/batch-metadata.json \
    -n 14 \
    -m 20000 \
    2>&1 | tee /home/jupyter/test_output/B026_20210317-1_log.txt
```

[Return to Contents](#contents)

<a id="scrna_report_out"></a>

#### Output Files

`run-scrna-batch-qc.R` will generate the HTML reporting file with name as defined by inut parameter -o. 

For example, using the run above, we would get the following outputs in out_dir:
```
B026_20210317.html
```

[Return to Contents](#contents)

<a id="scrna_report_test"></a>

#### Tests

Test runs can be performed using datasets provided with the `batchreporter` package using `-b X002`. These require only the `-t` and `-o` parameters.

```
Rscript --vanilla \
    /home/jupyter/batchreporter/run-scrna-batch-qc.R \
    -b X002  \
    -o /home/jupyter/test_output \

```

[Return to Contents](#contents)

