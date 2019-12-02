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
  spread(key = "meas", value = "value" ) %>%
  group_by(seqn) %>%
  summarise(alco = mean(alco), caff = mean(caff),
            fat = mean(fat), fibe = mean(fibe),
            iron = mean(iron), prot = mean(prot),
            sodi = mean(sodi), sugr = mean(sugr), water = mean(water))

demo = demo %>%
  # transmute(seqn = seqn, age = ridageyr, pir = indfmpir, gender = riagendr)
  transmute(seqn = seqn, gender = riagendr)

bpx = bpx %>%
  # most people havn't test 4 so we just ignore it
  transmute(seqn = seqn, bpxsy_avg = (bpxsy1+bpxsy2+bpxsy3)/3,
            bpxdiff_avg = ((bpxsy1-bpxdi1)+(bpxsy2-bpxdi2)+(bpxsy3-bpxdi3))/3)

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
df_scale = scale(df[,2:12])

df = df %>%
  select(seqn, gender) %>%
  transmute(seqn, gender = gender-1.5) %>%
  cbind(df_scale) %>%
  mutate(gender_alco = gender*alco,
         gender_caff = gender*caff,
         gender_fat = gender*fat,
         gender_fibe = gender*fibe,
         gender_iron = gender*iron,
         gender_prot = gender*prot,
         gender_sodi = gender*sodi,
         gender_sugr = gender*sugr,
         gender_water = water*gender)

# -0.5 represents male and 0.5 represents female 

# write.csv(df, file = "df.csv")
  
library("glmnet")

# for bpxsy as response 

penalty.factor = append(0,rep(1, 18))

m1 = glmnet(x = as.matrix(df[,c(2:11,15:22)]), 
            y = as.matrix(df[,c(12)]), 
            family="gaussian", alpha = 1, penalty.factor = penalty.factor)
mod_cv1 <- cv.glmnet(x=as.matrix(df[,c(2:11,15:22)]), y=as.matrix(df[,c(12)]), 
                     type.measure = "mse", family='gaussian', parallel = TRUE, penalty.factor = penalty.factor)
coef(m1, s=c(mod_cv1$lambda.min))

mod_cv1$lambda.min
m11 = lm(as.matrix(df[,c(12)])~as.matrix(df[,c(2:11, 15:22)]))



plot(m1)

# using DL Data


df = read.csv('../../DL/group_2_stata.csv')
df = df  %>%
  mutate(gend_alco = gend*alcohol_s,
         gend_caff = gend*caff_s,
         gend_fat = gend*fat_s,
         gend_fibe = gend*fiber_s,
         gend_iron = gend*iron_s,
         gend_prot = gend*protein_s,
         gend_sodi = gend*sodium_s,
         gend_sugr = gend*sugar_s,
         gend_water = water_s*gend)

m1 = glmnet(x = as.matrix(df[,c(4:12,14:23)]), 
            y = as.matrix(df[,c(2)]), 
            family="gaussian", alpha = 1, penalty.factor = penalty.factor)
mod_cv1 <- cv.glmnet(x=as.matrix(df[,c(4:12,14:23)]), y=as.matrix(df[,c(2)]), 
                     type.measure = "mse", family='gaussian', parallel = TRUE, penalty.factor = penalty.factor)
coef(m1, s=c(mod_cv1$lambda.min))

mod_cv1$lambda.min
m11 = lm(as.matrix(df[,c(12)])~as.matrix(df[,c(2:11, 15:22)]))


# eliminate the predictor without gender

# m2 = glmnet(x = as.matrix(df[,c(3,15:23)]), 
#             y = as.matrix(df[,c(13:14)]), 
#             family="mgaussian", alpha = 1, penalty.factor = penalty.factor)
# mod_cv2 <- cv.glmnet(x=as.matrix(df[,c(3,15:23)]), y=as.matrix(df[,c(13:14)]), 
#                      type.measure = "mse", family='mgaussian', parallel = TRUE, penalty.factor = penalty.factor)
# coef(m2, s=c(mod_cv2$lambda.min, m2$lambda[15]))
# 
# mod_cv2$lambda.min
# m22 = lm(as.matrix(df[,c(13:14)])~as.matrix(df[,c(15:23)]))
# m23 = glm(as.matrix(df[,c(13)])~as.matrix(df[,c(3:12)]))
# m24 = glm(as.matrix(df[,c(14)])~as.matrix(df[,c(3:12)]))
# plot(m2)
 

