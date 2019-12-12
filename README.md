# STATS 506 Group Project F19

- [STATS 506 Group Project F19](#stats-506-group-project-f19)
  - [Author: Group 2](#author-group-2)
  - [Navigation](#navigation)
    - [Report](#report)
    - [Data Sets](#data-sets)
    - [Code Reviews](#code-reviews)
    - [Scripts](#scripts)
  - [Overview](#overview)
  - [Method: LASSO with customized penalty.factor](#method-lasso-with-customized-penaltyfactor)
  - [Requirements](#requirements)
    - [Install Statamarkdown](#install-statamarkdown)

## Author: Group 2
Diana Liang ([STATA](./DL/Final&#32;Documents/))

Sijun Zhang ([dplyr_glmnet](./ZSJ/FINAL/))

Wenjing Zhou ([data.table_customized_cross-validation](./Wenjing/))

## Navigation

### Report
The completed *report* in .html and .Rmd files are stored in **[Final](./Final)** folder.

### Data Sets
We’re using a combination of 4 datasets (Demographics, Blood Pressure, Total Nutrients day 1, Total Nutrients day 2) from the 2015-2016 NHANES, which are stored in **[DATA](./DATA)** folder.

### Code Reviews

We are working on both the [issues](https://github.com/Randyzhang98/STATS506_Proj_02/issues) and commit code reviews. Here some instances about code reviews [README.md](https://github.com/Randyzhang98/STATS506_Proj_02/commit/70771c6ef62de5f1877e45f5a53b359115866a12), [STATA_LASSO_penalty](https://github.com/Randyzhang98/STATS506_Proj_02/commit/71b27f4d5a3b6003d2af6e66a4768863589c4357), [README.md_formula_editing](https://github.com/Randyzhang98/STATS506_Proj_02/commit/34689b24aa2958a0b710594bec6451b24e4fd0c1#commitcomment-36292581) and [Interactions_glmnet](https://github.com/Randyzhang98/STATS506_Proj_02/commit/ad8a4ace44e241fb9b1add7b817df005a3d1280f).

### Scripts

The final scripts path are linked in the **Author** part and you can find running guidence in each folder's readme.md.

## Overview
 We choose to investigate whether both genders will react the same under the the effect of consumption habits on blood pressure, a known symptom of different chronic diseases. Will the factors that are most important in determining blood pressure be different between males the females?

>**Whether both genders will react the same under the the effect of consumption habits on blood pressure**

 We will show in the following analysis that there are certain foods that affect one gender more than the other, and that these foods change depending on the type of blood pressure being measured.

## Method: LASSO with customized penalty.factor

Since our purpose is to find how gender effect the relationship between the nutrition intake and the blood pressure measurement, we only penalize the interaction terms by setting the penalty.factor of the interaction terms with a same positive value and let other terms' zero. The explict form for LASSO minizing goal is shaped to

<div align=center>
<a href="https://www.codecogs.com/eqnedit.php?latex=\underset{\beta}{\operatorname{minimize}}&space;\quad&space;\frac{1}{2}&space;\frac{\operatorname{RSS}}{n}&plus;&space;\lambda&space;\sum_{j=1}^{p}&space;\frac{c_{j}}{\bar{c}}\left\|\beta_{j}\right\|_{1}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\underset{\beta}{\operatorname{minimize}}&space;\quad&space;\frac{1}{2}&space;\frac{\operatorname{RSS}}{n}&plus;&space;\lambda&space;\sum_{j=1}^{p}&space;\frac{c_{j}}{\bar{c}}\left\|\beta_{j}\right\|_{1}" title="\underset{\beta}{\operatorname{minimize}} \quad \frac{1}{2} \frac{\operatorname{RSS}}{n}+ \lambda \sum_{j=1}^{p} \frac{c_{j}}{\bar{c}}\left\|\beta_{j}\right\|_{1}" /></a>
</div>

where <img src="http://chart.googleapis.com/chart?cht=tx&chl= c_j" style="border:none;"> is the penalty factor we used for each term

## Requirements

To run the group_2_final.Rmd file in Draft folder, the following packages should be pre-installed in the IDE.

| Version | Package            |
| ------- | ------------------ |
| 3.6.1   | dplyr              |
| 3.6.1   | ggplot2            |
| 2.1.3   | tibble             |
| 1.3.1   | readr              |
| 1.0.0   | tidyr              |
| 1.4.0   | stringr            |
| 0.4.0   | forcats            |
| 4.3-0   | Hmisc              |
| 1.6.0   | SASxport           |
| 2.0-18  | glmnet             |
| 1.12.2  | data.table         |
| 0.4.0   | Statamarkdown      |
| 1.1.0   | kableExtra         |

### Install Statamarkdown
As *Statamarkdown* doesn't support direct downloading using install.packages(), we can use the devtools package to install it from github.com.

```{r}
library(devtools) # before this you may need to install devtools
install_github("hemken/Statamarkdown")
```

If for some reason that gives you problems, you can also install from this website

```{r}
# For Windows
install.packages("https://www.ssc.wisc.edu/~hemken/Stataworkshops/Stata%20and%20R%20Markdown/Statamarkdown_0.3.9.zip", repos=NULL)

# For linux or Mac
install.packages("https://www.ssc.wisc.edu/~hemken/Stataworkshops/Stata%20and%20R%20Markdown/Statamarkdown_0.3.9.tar.gz", type="source", repos=NULL)
```



