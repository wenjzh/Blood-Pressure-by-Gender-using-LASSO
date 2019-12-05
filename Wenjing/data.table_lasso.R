# R using data.table and glmnet

# Part of Final Project of STATS 506
# Git Repo:https://github.com/Randyzhang98/STATS506_Proj_02
# Author: Wenjing Zhou
# 80:--------------------------------------------------------------------------

# Libraries: ------------------------------------------------------------------
library(Hmisc)
library(data.table)
library(glmnet)
library(SASxport)

# read in the  data: ----------------------------------------------------------
url_base <- 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/'

demo_file <- '../DATA/DEMO_I.XPT'
if ( !file.exists(demo_file) ) {
  demo_url <- sprintf('%s/DEMO_I.XPT', url_base)
  demo <- sasxport.get(demo_url)
  write.xport(demo, file = demo_file)
} else {
  demo <- sasxport.get(demo_file)
}

dr1_file <- '../DATA/DR1TOT_I.XPT'
if ( !file.exists(dr1_file) ) {
  dr1_url <- sprintf('%s/DR1TOT_I.XPT', url_base)
  dr1 <- sasxport.get(dr1_url)
  write.xport(dr1, file = dr1_file)
} else {
  dr1 <- sasxport.get(dr1_file)
}

dr2_file <- '../DATA/DR2TOT_I.XPT'
if ( !file.exists(demo_file) ) {
  dr2_url <- sprintf('%s/DR2TOT_I.XPT', url_base)
  dr2 <- sasxport.get(dr2_url)
  write.xport(dr2, file = dr2_file)
} else {
  dr2 <- sasxport.get(dr2_file)
}

bpx_file <- '../DATA/BPX_I.XPT'
if ( !file.exists(bpx_file) ) {
  bpx_url <- sprintf('%s/BPX_I.XPT', url_base)
  bpx <- sasxport.get(bpx_url)
  write.xport(bpx, file = bpx_file)
} else {
  bpx <- sasxport.get(bpx_file)
}

dr1 <-as.data.table(dr1)
dr2 <-as.data.table(dr2)
demo <-as.data.table(demo)
bpx <-as.data.table(bpx)


# Choose variables: -----------------------------------------------------------
dr1 = dr1[,.(seqn, dr1talco,dr1.320z,dr1tcaff,dr1tsodi, 
             dr1ttfat,dr1tsugr,dr1tiron,dr1tfibe,dr1tprot)] 
dr2 = dr2[,.(seqn, dr2talco,dr2.320z,dr2tcaff,dr2tsodi,
             dr2ttfat,dr2tsugr,dr2tiron,dr2tfibe,dr2tprot)]
demo = demo[,.(seqn = seqn, age = ridageyr, pir = indfmpir, gender = riagendr)]
bpx = bpx[,.(seqn = seqn, 
             # most people havn't test 4 so we just ignore it
             systolic = (bpxsy1+bpxsy2+bpxsy3)/3,
             diastolic = (bpxdi1+bpxdi2+bpxdi3)/3)][,
                                                    diff:=systolic-diastolic
                                                    ]

# Combine the four data tables and drop the missing values
mydata=dr1[dr2,on = 'seqn'][demo,, on = 'seqn'][bpx,,on = 'seqn'][,
        .(id = seqn, systolic, diastolic, diff, gender,
        alco = (dr1talco+dr2talco)/2, water = (dr1.320z+dr2.320z)/2, 
        caff = (dr1tcaff+dr2tcaff)/2, sodi = (dr1tsodi+dr2tsodi)/2, 
        fat = (dr1ttfat+dr2ttfat)/2, sugr = (dr1tsugr+dr2tsugr)/2, 
        iron = (dr1tiron+dr2tiron)/2, fibe = (dr1tfibe+dr2tfibe)/2, 
        prot = (dr1tprot+dr2tprot)/2)][! is.na(id) & ! is.na(systolic) &
        ! is.na(diastolic) & ! is.na(gender) & ! is.na(alco) &
        ! is.na(water) & ! is.na(caff) & ! is.na(sodi) & 
        ! is.na(fat) & ! is.na(sugr) & ! is.na(iron) &
        ! is.na(fibe) & ! is.na(prot), ]

# Normalize the nutrition predictors
mydata[,6:14] <- lapply(mydata[,6:14], function(x) c(scale(x)))

# Change gender 1/2 to -0.5/0.5
mydata = mydata[, gender := fifelse(gender==1, 0.5, -0.5)]

# Interaction Terms
mydata = mydata[, .(systolic, diff, gender,alco,water,caff,sodi,fat,sugr,iron,fibe,prot,
                    gender_alco = gender*alco,
                    gender_water = gender*water,
                    gender_caff = gender*caff,
                    gender_sodi = gender*sodi,
                    gender_fat = gender*fat,
                    gender_sugr = gender*sugr,
                    gender_iron = gender*iron,
                    gender_fibe = gender*fibe,
                    gender_prot = gender*prot)]

# Penalty Weights
pnfc=c(rep(0,10),rep(1,9))

# Cross Validation: -----------------------------------------------------------
# LASSO using glmnet package: alpha = 1 
mydata = as.matrix(mydata)

set.seed(000)
cv_syst = cv.glmnet(x=mydata[,3:21],y=mydata[,1], type.measure="mse", 
                    family="gaussian",penalty.factor=pnfc,alpha = 1)
lambda_syst = cv_syst$lambda.min

cv_diff = cv.glmnet(x=mydata[,3:21],y=mydata[,2], type.measure="mse",
                    family="gaussian", penalty.factor=pnfc,alpha = 1)
lambda_diff = cv_diff$lambda.min

plot(cv_syst$lambda, cv_syst$cvm, ylab = "Mean standard error", xlab = "lambda",
     main = "Cross-validation for lambda in systolic pressure")
abline(v = cv_syst$lambda.min)

plot(cv_diff$lambda, cv_diff$cvm, ylab = "Mean standard error", xlab = "lambda",
     main = "Cross-validation for lambda in pressure difference")
abline(v = cv_diff$lambda.min)

# Modeling: -------------------------------------------------------------------
lars_syst = 
  glmnet(x=mydata[,3:21],y=mydata[,1], penalty.factor=pnfc, family="gaussian",
         lambda=lambda_syst, alpha = 1,nlambda=100)

lars_diff = 
  glmnet(x=mydata[,3:21],y=mydata[,2], penalty.factor=pnfc, family="gaussian",
         lambda=lambda_diff, alpha = 1,nlambda=100)

# Results: --------------------------------------------------------------------
# data collection
coef1 = coef(lars_syst)
coef2 = coef(lars_diff)
coef = cbind(coef1, coef2)
lambda = cbind(lambda_syst, lambda_diff)
rownames(lambda) = "lambda_min_mse"
coef = rbind(coef, lambda)
colnames(coef) = c("systolic", "difference")

coef

















