#!/bin/bash/

Rscript --vanilla \
    /home/jupyter/scrna-batch-report/run-scrna-batch-qc.R \
    -b T001 \
    -d /home/jupyter/test_output/ \
    -o  test.html
    