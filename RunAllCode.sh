#! /bin/bash

#Check where the rows of RNA with values and dates of measurement and without Patient ID come from. 


python IeDEA_Datamerge.py
echo "Done with merging & preparation of initial Datasets"   
# Checked
# Input are merged Adult dataset and three children dateset to be merged.
# Outputs merged datasets that have a viral load column with similar dates for rna_dmy and lab_dmy on each row.


python transform_v0.3.py "/home/evaliliane/Documents/PhD/Python/AdultData.csv" 1
echo "Done with adding time to Adult Dataset"

python transform_v0.3.py "/home/evaliliane/Documents/PhD/Python/ChildrenData.csv" 2
echo "Done with adding time to Children Dataset"
# Checked
# Input are AdultData.csv and ChildrenData.csv.
# Outputs datasets that have "diff" variable that is time since HAART initiation.

python regression.py "/home/evaliliane/Documents/PhD/Python/AdultData_Time.csv" 1
echo "Done with adding slope to Adult Dataset"

python regression.py "/home/evaliliane/Documents/PhD/Python/ChildrenData_Time.csv" 2
#echo "Done with adding slope to Children Dataset"
# Checked
# Input are AdultData_Time.csv and ChildrenData_Time.csv.
# Outputs datasets that have "diff" variable that is time since HAART initiation.

python IsBaseline.py "/home/evaliliane/Documents/PhD/Python/AdultData_Time_Slope.csv" 1
echo "Done with adding Baseline info to Adult Dataset"

python IsBaseline.py "/home/evaliliane/Documents/PhD/Python/ChildrenData_Time_Slope.csv" 2
echo "Done with adding Baseline info to Children Dataset"

python Transform_CD4-Zscores.py
echo "Done with adding CD4 Zscore to Children Dataset"


# BMI & Weight and Height !!!
