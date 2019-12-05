library(glmnet)
didata = fread('/Users/wenjzh/Desktop/506_proj/STATS506_Proj_02/DL/group_2_stata.csv')

data=data.frame(x=0:10,y=10:20,z=5:15)
lm(z~x*y,data)
