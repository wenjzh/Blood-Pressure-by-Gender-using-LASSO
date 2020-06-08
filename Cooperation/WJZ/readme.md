# Wenjing Zhou using data.table and glmnet

This the working path for LASSO fitting using data.table and glmnet

## Install "doMC" package from the internet

Run the code in Rstudio to install the package first, then you should be able to require or library it.
install.packages("https://cran.r-project.org/src/contrib/doMC_1.3.6.tar.gz",
                 repos = NULL, type = "source")

## Run the script

After meeting the package requirements, the data_table_final.Rmd could be directly excuted, which contains the full version for data.table & glmnet & parallel computing. 

## Random status control

As cross-validation introduced randomization in parameter selection, the randomization seed is reset to be 000 everytimes cv_lambda() function is called.
