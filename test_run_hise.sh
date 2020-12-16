#!/bin/bash/

Rscript --vanilla \
    /home/jupyter/scrna-batch-report/run-scrna-batch-qc-hise.R \
    -d /home/jupyter/test_output/ \
    -o  test_B002P1_20201215.html \
    2>&1 | tee /home/jupyter/test_output/test_20201215_log.txt
    