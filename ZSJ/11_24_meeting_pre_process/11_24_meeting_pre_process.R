# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT
# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPX_I.XPT
# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.XPT
# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR2TOT_I.XPT

library(tidyverse)
library(Hmisc)
library(SASxport)

# read in the  data: -----------------------------------------------------------
## This data will be used in the question. 
url_base <- 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/'

demo_file <- '../../DATA/DEMO_I.XPT'
if ( !file.exists(demo_file) ) {
  demo_url <- sprintf('%s/DEMO_I.XPT', url_base)
  demo <- sasxport.get(demo_url)
  write.xport(demo, file = demo_file)
} else {
  demo <- sasxport.get(demo_file)
}

dr1_file <- '../../DATA/DR1TOT_I.XPT'
if ( !file.exists(dr1_file) ) {
  dr1_url <- sprintf('%s/DR1TOT_I.XPT', url_base)
  dr1 <- sasxport.get(dr1_url)
  write.xport(dr1, file = dr1_file)
} else {
  dr1 <- sasxport.get(dr1_file)
}

dr2_file <- '../../DATA/DR2TOT_I.XPT'
if ( !file.exists(demo_file) ) {
  dr2_url <- sprintf('%s/DR2TOT_I.XPT', url_base)
  dr2 <- sasxport.get(dr2_url)
  write.xport(dr2, file = dr2_file)
} else {
  dr2 <- sasxport.get(dr2_file)
}

bpx_file <- '../../DATA/BPX_I.XPT'
if ( !file.exists(bpx_file) ) {
  bpx_url <- sprintf('%s/BPX_I.XPT', url_base)
  bpx <- sasxport.get(bpx_url)
  write.xport(bpx, file = bpx_file)
} else {
  bpx <- sasxport.get(bpx_file)
}

dr1 = dr1 %>%
  transmute(seqn = seqn, alco = as.numeric(dr1talco),
            water = as.numeric(dr1.320z), caff = as.numeric(dr1tcaff),
            sodi = as.numeric(dr1tsodi), fat = as.numeric(dr1ttfat), 
            sugr = as.numeric(dr1tsugr), iron = as.numeric(dr1tiron),
            fibe = as.numeric(dr1tfibe), prot = as.numeric(dr1tprot) ) %>%
  gather(key = "meas", value = "day1", -seqn )

dr2 = dr2 %>%
  transmute(seqn = seqn, alco = as.numeric(dr2talco),
            water = as.numeric(dr2.320z), caff = as.numeric(dr2tcaff), 
            sodi = as.numeric(dr2tsodi), fat = as.numeric(dr2ttfat), 
            sugr = as.numeric(dr2tsugr), iron = as.numeric(dr2tiron),
            fibe = as.numeric(dr2tfibe), prot = as.numeric(dr2tprot) ) %>%
  gather(key = "meas", value = "day2", -seqn )

dr = dr1 %>%
  left_join(dr2, by = c('seqn', 'meas'))  %>%
  gather(key = "svy_day", value = "value", day1:day2) %>%
  spread(key = "meas", value = "value" )

demo = demo %>%
  # transmute(seqn = seqn, age = ridageyr, pir = indfmpir, gender = riagendr)
  transmute(seqn = seqn, gender = riagendr)

bpx = bpx %>%
  # most people havn't test 4 so we just ignore it
  transmute(seqn = seqn, bpxsy_avg = (bpxsy1+bpxsy2+bpxsy3)/3,
            bpxdi_avg = (bpxdi1+bpxdi2+bpxdi3)/3)

bpx_dr = dr %>%
  left_join(bpx, by="seqn")


df = bpx_dr %>%
  left_join(demo, by = "seqn") %>%
  drop_na()

# normalizing process
my_scale = function(x) {
  re = x %>%
    mutate(id = row_number()) %>%
    gather(key = "meas", value = "value", -id) %>%
    group_by(meas) %>%
    mutate(avg = mean(value),
           sd = sd(value)) %>%
    mutate(value = (value - avg)/sd ) %>%
    select(id, meas, value) %>%
    spread(key = "meas", value = "value") %>%
    select(-id)
  return(re)
} 

# df_scale = scale(df[,3:15])
df_scale = scale(df[,3:13])

df = df %>%
  select(seqn, svy_day, gender) %>%
  cbind(df_scale)

df_male = df %>%
  filter(gender == 1) %>%
  drop_na()
  

df_female = df %>%
  filter(gender == 2) %>%
  drop_na()
  
library("glmnet")

m1 = glmnet(x = as.matrix(df_male[,c(4:12)]), 
            y = as.matrix(df_male[,c(13:14)]), 
            family="mgaussian", alpha = 1)
mod_cv1 <- cv.glmnet(x=as.matrix(df_male[,c(4:12)]), y=as.matrix(df_male[,c(13:14)]), family='mgaussian', parallel = TRUE)
coef(m1, s=c(mod_cv1$lambda.1se, m1$lambda[15]))

mod_cv1$lambda.1se
m11 = lm(as.matrix(df_male[,c(13:14)])~as.matrix(df_male[,c(4:12)]))



W
m2 = glmnet(x = as.matrix(df_female[,c(4:12)]), 
            y = as.matrix(df_female[,c(13:14)]), 
            family="mgaussian", alpha = 1)
mod_cv2 <- cv.glmnet(x=as.matrix(df_female[,c(4:12)]), y=as.matrix(df_female[,c(13:14)]), family='mgaussian', parallel = TRUE)
coef(m2, s=c(mod_cv2$lambda.1se, m1$lambda[24]))
plot(m2)

m21 = lm(as.matrix(df_female[,c(13:14)])~as.matrix(df_female[,c(4:12)]))


