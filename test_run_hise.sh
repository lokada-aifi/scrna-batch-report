#!/bin/bash/

Rscript --vanilla \
    /home/jupyter/scrna-batch-report/run-scrna-batch-qc-hise.R \
    -b B001-P1-Test3  \
    -i /home/jupyter/test_data/B002_3samples/ \
    -k /home/jupyter/scrna-batch-report/inst/extdata/example_sample_key.csv  \
    -d /home/jupyter/test_output/ \
    -o  B002-P1_3samples_20210105-2.html \
    2>&1 | tee /home/jupyter/test_output/B002-P1_3samples_20210105-2_log.txt
    