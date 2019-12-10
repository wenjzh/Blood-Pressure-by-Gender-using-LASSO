# Wenjing Zhou using data.table and glmnet

This the working path for LASSO fitting using data.table and glmnet

## Run the script

After meeting the package requirements listed in [README.md](../../README.md), the data.table_lasso.R could be directly excuted. The data.table_lasso.Rmd also contains the complete version of codes.

## Random status control

As cross-validation introduced randomization in parameter selection, the randomization seed is reset to be 000 everytimes cv.glmnet() function is called.