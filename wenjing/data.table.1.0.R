

# Libraries: ------------------------------------------------------------------
library(Hmisc)
library(data.table)
library(glmnet)

# Directories: ----------------------------------------------------------------
setwd("/Users/wenjzh/Desktop/506_proj/STATS506_Proj_02/DATA")

# Data processing using data.table package: -----------------------------------
# Import data
bpx <- as.data.table(sasxport.get("BPX_I.XPT"))
demo <- as.data.table(sasxport.get("DEMO_I.XPT"))
dr1 <- as.data.table(sasxport.get("DR1TOT_I.XPT"))
dr2 <- as.data.table(sasxport.get("DR2TOT_I.XPT"))

# Choose variables
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
mydata=dr1[dr2,on = 'seqn'][demo, , on = 'seqn'][bpx,,on = 'seqn'][,
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
mydata = mydata[, gender := fifelse(gender==1, -0.5, 0.5)]

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

# LASSO using glmnet package: alpha = 1 ---------------------------------------
mydata = as.matrix(mydata)

# Only penalize the interaction terms
pnfc=c(rep(0,10),rep(1,9))

set.seed(1)

lambda_syst = cv.glmnet(x=mydata[,3:21],y=mydata[,1], type.measure="mse",
                        penalty.factor=pnfc,alpha = 1)$lambda.min

lambda_diff = cv.glmnet(x=mydata[,3:21],y=mydata[,2], type.measure="mse",
                        penalty.factor=pnfc,alpha = 1)$lambda.min

lars_syst = 
  glmnet(x=mydata[,3:21],y=mydata[,1], penalty.factor=pnfc, 
         lambda=lambda_syst, alpha = 1)

lars_diff = 
  glmnet(x=mydata[,3:21],y=mydata[,2], penalty.factor=pnfc, 
         lambda=lambda_diff, alpha = 1)


















