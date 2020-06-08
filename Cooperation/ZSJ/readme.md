# Sijun Zhang using dplyr and glmnet

This the working path for LASSO fitting using dplyr and glmnet

## Run the script

After meeting the package requirements listed in [README.md](../../README.md), the group_2_dplyr.R could be directly excuted. The group_2_dplyr.Rmd also contains the complete version of codes.

## Random status control

As cross-validation introduced randomization in parameter selection, the randomization seed is reset to be 1984 everytimes cv.glmnet() function is called.