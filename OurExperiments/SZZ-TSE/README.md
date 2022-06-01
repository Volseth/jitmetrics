# The Impact of Mislabeled Changes by SZZ on Just-in-Time Defect Prediction (Code and Data)

This repository provides the datasets and code for reproducing our paper - The Impact of Mislabeled Changes by SZZ on Just-in-Time Defect Prediction.

# Data Labelling
Our data is labeled using Daniel's implementaiton of B-SZZ, AG-SZZ, MA-SZZ and RA-SZZ. See the following repositories:

- Repository for B-SZZ, AG-SZZ, MA-SZZ: https://github.com/danielcalencar/ma-szz

- Repository for RA-SZZ: https://github.com/danielcalencar/raszzprime

- the file `labeled_data.zip` is calculated for all the apache projects using b-szz, ag-szz, ma-szz and ra-szz. 
  All the id are the CVS id of the commit. The CSV id can be retrieved from the commit message of each commit.

## How to run
* Clone the code to a directory. Running our code needs the path of the directory.
* Modify a line in the file `code/calculation/classification_importance.R`, the line is as follows:
```
# Specify the DIRECTORY path storing the code of this repository
DIR_PATH = "?" 
```
* Create the following directories in `data_results/`: `importance`, `oneway`, `results_imbalance`, `results_balance`.
* Run the script `code/calculation/classification_importance.R`

## Results
The results will be stored in `data_results/importance/`, `data_results/oneway/`,  `data_results/results_imbalance/`, `data_results/results_balance/` after running the script. We store the performance scores (e.g., auc) in each of the 1,000 bootstrap iterations into csv files.

## Citation
If you find our code useful for your research, please cite:

```
@article{fan2019impact, 
author={Y. {Fan} and X. {Xia} and D. {Alencar da Costa} and D. {Lo} and A. E. {Hassan} and S. {Li}}, 
journal={IEEE Transactions on Software Engineering}, 
title={The Impact of Changes Mislabeled by SZZ on Just-in-Time Defect Prediction}, 
year={2019},
doi={10.1109/TSE.2019.2929761}
}
```
