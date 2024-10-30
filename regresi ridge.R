library(car)
library(lmtest)
library(lmridge)
library(MASS)

X<-read.delim("clipboard")
Y<-PDRB
best.lr<-cv.r$lambda.min
bestridge<-glmnet(x,Y,alpha=0,lambda=best.lr)
summary(bestridge)
coef(bestridge)
rsq<-function(bestmodel,bestlambda,x,Y){
  y.duga <- predict(bestmodel, s = bestlambda, newx = x)
  
  #JKG dan JKT
  jkt <- sum((Y - mean(Y))^2)
  jkg <- sum((y.duga- Y)^2)
  
  #find R-Squared
  rsq <- 1 - jkg/jkt
  return(rsq) 
}
rsq(bestridge,best.lr,x,Y)
y_pred_ridge <- predict(bestridge, s = 0.99399, newx = x)  

RSE_ridge <- round(sqrt(sum((Y - y_pred_ridge)^2) / (length(Y) - ncol(x) - 1)),4)


#Sintaks kedua
ORR=function(x,Y,xbin,ybin) 
{ 
  #penentuan nilai c ORR 
  betamkt=(solve(t(xbin)%*%xbin))%*%(t(xbin)%*%ybin) 
  JKGmkt=((t(ybin-(xbin%*%betamkt)))%*%(ybin-(xbin%*%betamkt))) 
  KTGmkt=JKGmkt/(113) 
  cc=5*KTGmkt
  MKTG=diag((as.vector(KTGmkt)),9,9) 
  c=cc/(t(betamkt)%*%betamkt) 
  c=0.01
  c1=0.02
  c2=0.03
  co=diag((as.vector(c_hkb)),9,9) 
  c_hkb<-0.05719
  
  
  #pendugaan parameter ORR 
  betaorr=(solve((t(xbin)%*%xbin)+co))%*%(t(xbin)%*%ybin) 
  #mencari beta0
  b0=mean(ybin)-betaorr[1]*mean(xbin[,1])-betaorr[2]*mean(xbin[,2])- betaorr[3]*mean(xbin[,3])- betaorr[4]*mean(xbin[,4])- betaorr[5]*mean(xbin[,5])
    -betaorr[6]*mean(xbin[,6])- betaorr[7]*mean(xbin[,7])-betaorr[8]*mean(xbin[,8])- betaorr[9]*mean(xbin[,9])                                        )
  b0
  betaorrlengkap=matrix(c(b0,betaorr),10,1)
  #ragam penduga ORR 
  v=solve(t(xbin)%*%xbin+co) 
  varorr=diag(MKTG%*%v%*%(t(xbin)%*%xbin)%*%v)

  #VIF ORR
  viforr=diag(v%*%(t(xbin)%*%xbin)%*%v)
  #Uji Koefisien Determinasi
  koefdetridge = JKRo/JKTo
  koefdetridge
  #uji simultan 
  x0=rep(1,123) 
  xx=cbind(x0,xbin)
  JKRo=t(betaorrlengkap)%*%t(xx)%*%ybin
  JKTo=t(ybin)%*%ybin
  JKSo=JKTo-JKRo
  fohitung=(JKRo/(9))/(JKSo/(113))
#menghitung nilai MSE
  MSEridge = JKSo/113
#berdasarkan nilai MSE metode jackknife ridge yg paling baik
  #uji parsial 
  to1=betaorrlengkap[2,]/sqrt(varorr[1]) 
  to2=betaorrlengkap[3,]/sqrt(varorr[2])
  to3=betaorrlengkap[4,]/sqrt(varorr[3])
  to4=betaorrlengkap[5,]/sqrt(varorr[4])
  to5=betaorrlengkap[6,]/sqrt(varorr[5])
  biass = betamkt-co%*%v%*%betamkt
 
  #variabel yg signifikan X1,X2,X4
  #mengembalikan penduga dalam model regresi 
  bo1=(sd(Y)/sd(x[,1]))*betaorr[1]
  bo2=(sd(Y)/sd(x[,2]))*betaorr[2]
  bo4=(sd(Y)/sd(x[,4]))*betaorr[4]
  bo0=mean(Y)-betaorr[1]*mean(x[,1])-betaorr[2]*mean(x[,2])- betaorr[4]*mean(x[,4])
  betaorrbalik=matrix(c(bo0,bo1,bo2,bo4),4,1)
  
  

#sintaks regresi ridge dengan function ridge
pairs(ybin~.,data = datatransform)
plot(lm.ridge(ybin~.,data = datatransform, lambda=seq(0, 0.1,0.0001)) )
centering_scalling<-scale(DATA)

reggg<-lm(ybinn~.,data=datatransformmm)
summary(reggg)

nilai_bias<-lmridge(datatransformmm$ybinn~.,data = datatransformmm)
summary(nilai_bias)
kest(nilai_bias)

#estimasi regresi ridge dengan bias HKB
model1<-lmridge(datatransformmm$ybinn~., data = datatransformmm,K=c(0.21729))
summary(model1)
vif(model1)
#contoh untuk membuktikan beta transpos kali beta transpose = c2
betaar = matrix(c(0.4166,0.1126,0.1229,-0.2489,0.0628,0.0793,-0.0121,0.1025,
                  0.0956), 9)
hasilll <- t(betaar)%*%betaar
Cd=0.0605^2
vif(model1)
#estimasi parameter ridge dengan bias LW (1976)
model2<-lmridge(datatransform$ybin~.,data = datatransform,K=c(0.11810))
summary(model2)
#Transformasi data kebentuk awal dengan menggunakan seluruh variabel prediktor (METODE regresi ridge)
B1= (sd(Y)/sd(x1))%*%0.3217
B2 = (sd(Y)/sd(x2))%*%0.1524
B3 = (sd(Y)/sd(x3))%*%0.1094
B4= (sd(Y)/sd(x4))%*%-0.2111
B5 = (sd(Y)/sd(x5))%*%0.0613
B6 = (sd(Y)/sd(x6))%*%0.0838
B7 = (sd(Y)/sd(x7))%*%-0.0118
B8 = (sd(Y)/sd(x8))%*%0.1057
B9 = (sd(Y)/sd(x9))%*%0.0979
#Menentukan beta 0 metode ridge
B0 = mean(Y)-(B1%*%mean(x1))-(B2%*%mean(x2))-(B3%*%mean(x3))-(B4%*%mean(x4))-
  (B5%*%mean(x5))-(B6%*%mean(x6))-(B7%*%mean(x7))-(B8%*%mean(x8))-(B9%*%mean(x9))
B0
bridgee= c(B0,B1,B2,B3,B4,B5,B6,B7,B8,B9)
BETARIDGE <- as.matrix(bridgee)
round(BETARIDGE,7)
betaa<-model_awal2
model_awal2<-lmridge(Y~x1+x2+x3+x4+x5+x6+x7+x8+x9, data = DATA,K=c(0.11810))
summary(model_awal2)
vif(model2)
#mencari nilai mse dari data testing
DATA_testing<-read.delim("clipboard")
summary(DATA_testing)
dim(DATA_testing)
# Membuat prediksi menggunakan model regresi pada data testing
predicted_testridge <- B0 +B1*DATA_testing$X1+B2*DATA_testing$X2+B3*DATA_testing$X3+B4*DATA_testing$X4+
  B5*DATA_testing$X5+B6*DATA_testing$X6+B7*DATA_testing$X7+B8*DATA_testing$X8+B9*DATA_testing$X9

predicted_testridge <- 16.62374 +0.16055*DATA_testing$X1+0.25599*DATA_testing$X2+0.00809*DATA_testing$X3-0.37076*DATA_testing$X4+
  0.09797*DATA_testing$X5+0*DATA_testing$X6+0.00001*DATA_testing$X7+0.00017*DATA_testing$X8+0*DATA_testing$X9

# Menghitung residual antara nilai sebenarnya dan prediksi pada data testing
residuals_test1 <- DATA_testing$Y - predicted_testridge 
residuals_test2 <- DATA_testing$Y - predicted_testridge 
# Menghitung MSE dari residual pada data testing
mse_test1 <- mean(residuals_test1^2)#(menggunakan hkb)
mse_test1
mse_test2 <- mean(residuals_test2^2)#(menggunakan LW 1976)
mse_test2
