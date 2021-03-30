#!/bin/bash/

Rscript --vanilla \
    /home/jupyter/batchreporter/run-scrna-batch-qc.R \
    -b B002  \
    -i /home/jupyter/test_data/B002_3samples/ \
    -k /home/jupyter/test_data/B002_3samples/sample_key_B002_3samples.csv   \
    -c /home/jupyter/test_data/B002_3samples/default_rna_config_v1.csv  \
    -d /home/jupyter/test_output/ \
    -o  B002-P1_3samples.html \
    -n 14 \
    -m 20000 \
    2>&1 | tee /home/jupyter/test_output/B002-P1_3samples_log.txt

    