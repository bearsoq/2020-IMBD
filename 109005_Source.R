install.packages("glment")
install.packages("mice")
library(glmnet)
library(mice)
data = read.table("C:/Users/sdfe0/Desktop/0714train.csv", sep = ",", header = TRUE, fill=TRUE)

data[data==""]=NA
data=na.omit(data)

col_tran = c(160:183,208:227)
names_tran = names(data[,col_tran])
nvar = length(names_tran)
data1 = data[,col_tran]
data2 = data[,-col_tran]

var_trans = function(x){
  x2 = as.numeric(x[4])
  y2 = as.numeric(x[2])
  if(x[3]=="L"){zx = -1*x2}
  else {zx = x2}
  if(x[1]=="D"){zy = -1*y2}
  else {zy = y2}
  r = sqrt(zx^2+zy^2)
  if(zx==0&&zy==0){ang = 0}
  else{ang = atan2(zy,zx)}
  var = c(zx,zy,r,ang)
  return(var)
}

for(i in 1:nvar){
  a = t(data.frame(strsplit(as.character(data1[,i]),";")))
  b = t(apply(a,1,var_trans))
  data2 = cbind(data2,b) 
}

nv = NULL
for(i in 1:nvar){
  nv = cbind(nv,paste(names_tran[i],c("xcoord","ycoord","rad","ang"), sep = "_"))
} 
nv = matrix(nv,nrow = 1, ncol = 4*nvar)
name_var = c(names(data2[,1:244]),nv)
names(data2) = name_var


names_imp = c("Input_A6_024","Input_A3_016","Input_C_013","Input_A2_016","Input_A3_017","Input_C_050","Input_A6_001","Input_C_096","Input_A3_018","Input_A6_019","Input_A1_020","Input_A6_011","Input_A3_015","Input_C_046","Input_C_049","Input_A2_024","Input_C_058","Input_C_057","Input_A3_013","Input_A2_017")
data_imp = data2[,names_imp]
data_x = data2[,!names(data2)%in%names_imp]

data_generating = function(names_pred){
  dataY = data_imp[,names_pred]
  a = data.frame(strsplit(names_pred,"_"))[2,]
  if(a=="C"){
    dataX = data_x[,-1]
  }
  else
  {
    n = c(grep(a,names(data_x)),grep("C",names(data_x)))
    dataX = data_x[,n]
  }
  data = data.frame(dataX,dataY)
  return(data)
}

library(glmnet)
#library(e1071)
modelling = function(names_pred){
  data_model = data_generating(names_pred)
  lasso = glmnet(x = as.matrix(data_model[,-ncol(data_model)]),y = as.matrix(data_model[,ncol(data_model)]), alpha = 1, family = "gaussian")
  cv.lasso = cv.glmnet(x = as.matrix(data_model[,-ncol(data_model)]),y = as.matrix(data_model[,ncol(data_model)]), alpha = 1, family = "gaussian")
  select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
  select.ind = select.ind[-1]-1 
  select.varialbes = colnames(data_model)[select.ind]
  select.varialbes
  best.lasso.lambda = cv.lasso$lambda.min
  pred1 = predict(lasso, s = best.lasso.lambda, newx = as.matrix(data_model[, -ncol(data_model)]))
  SSE1 = sum((data_model$dataY-pred1)^2)
  lm = lm(dataY ~ ., data = data_model[, c(select.varialbes, "dataY")])
  pred2 = lm$fitted.values
  SSE2 = sum((data_model$dataY-pred2)^2)
  return(lm)
}

testdata = read.table("C:/Users/sdfe0/Desktop/0728test.csv", sep = ",", header = TRUE, fill=TRUE)
library(mice)
library(Hmisc)

testdata[testdata==""]=NA
col_test_tran = c(146:169,189:208)
md.pattern(testdata)
col_na_categ = c(148,166:169, 189:208)
col_na_conti = c(209:217)

for(i in col_na_categ){
  testdata[,i] = impute(as.factor(testdata[,i]))
}
miceinterp = mice(testdata, method = "pmm", m = 5, maxit = 5, seed = 500)
miceOutput = complete(miceinterp,action = 1)
testdata1 = miceOutput
tcol_tran <- c(146:169,189:208)#待處理的位置變數
tnames_tran <- names(testdata1[,tcol_tran])
tnvar <- length(tnames_tran)
tdata1 <- testdata1[,tcol_tran]
tdata2 <- testdata1[,-tcol_tran]

for(i in 1:tnvar){
  a = t(data.frame(strsplit(as.character(tdata1[,i]),";")))
  b = t(apply(a,1,var_trans))
  tdata2 = cbind(tdata2,b) 
}

tnv = NULL
for(i in 1:tnvar){
  tnv = cbind(tnv,paste(tnames_tran[i],c("xcoord","ycoord","rad","ang"), sep = "_"))
} 
tnv = matrix(tnv,nrow = 1, ncol = 4*tnvar)
tname_var = c(names(tdata2[,1:224]),tnv)
names(tdata2) = tname_var
test2<-tdata2#處理好的測驗資料當預測資料

modelling = function(names_pred){
  data_model = data_generating(names_pred)
  lasso = glmnet(x = as.matrix(data_model[,-ncol(data_model)]),y = as.matrix(data_model[,ncol(data_model)]), alpha = 1, family = "gaussian")
  cv.lasso = cv.glmnet(x = as.matrix(data_model[,-ncol(data_model)]),y = as.matrix(data_model[,ncol(data_model)]), alpha = 1, family = "gaussian")
  select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
  select.ind = select.ind[-1]-1 
  select.varialbes = colnames(data_model)[select.ind]
  select.varialbes
  best.lasso.lambda = cv.lasso$lambda.min
  pred1 = predict(lasso, s = best.lasso.lambda, newx = as.matrix(data_model[, -ncol(data_model)]))
  SSE1 = sum((data_model$dataY-pred1)^2)
  lm = lm(dataY ~ ., data = data_model[, c(select.varialbes, "dataY")])
  pred2 = lm$fitted.values
  SSE2 = sum((data_model$dataY-pred2)^2)
  mylist = list("Lasso" = lasso , "CVLasso" = cv.lasso)
  return(mylist)
}
Predicctlassolm<-function(names_pred,newdataset){
  models<-modelling(names_pred)
  VL<-list(names(models$coefficients[-1]))
  VL<-unlist(VL)
  x<-newdataset[,VL]
  Yhat<-predict(models,x)
  return(Yhat)
}
aaa<-Predicctlassolm("Input_A6_024",test2)
bbb<-Predicctlassolm("Input_C_057",test2)
ccc<-NULL
for (i in names_imp) {
  aaa = Predicctlassolm(i,test2)
  ccc = cbind(ccc,aaa)
}

Finaldata<-data.frame(ccc)
colnames(Finaldata) = names_imp
rownames(Finaldata) = c(1:95)

write.table(Finaldata,file="C:/Users/sdfe0/Desktop/Finaldata.csv",sep=",",row.names=F, na = "NA")
