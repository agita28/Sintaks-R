library(olsrr)
library(car)
library(lmtest)
DATA<-read.delim("clipboard")
summary(DATA)
dim(DATA)
#DATA BARU
Y1<-DATA$TPT
x11<-DATA$IPM
x21<-DATA$RLS
x31<-DATA$jumlah.penduduk.miskin
x41<-DATA$UHH
x51<-DATA$Laju.PDRB
x61<-DATA$jumlah.penduduk
x71<-DATA$Pengeluaran.perkapita
x81<-DATA$Kepadatan.Penduduk
x91<-DATA$jumlah.bukan.angkatan.kerja

summary(DATA)
boxplot(DATA$TPT)
z_score <- (x31- mean(x31)) / sd(x31)
z_score <- (x3_log- mean(x3_log)) / sd(x3_log)
#membuat boxplot
par(mfrow=c(3,3))
setwd("D:\\Skripsi")
png("sctterplot variabel.png",units = "in",width = 7, height = 4,res = 800)
dev.off()
boxplot(DATA$TPT,
        main = "Boxplot TPT",    # Judul Boxplot
        xlab = "TPT",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "indianred")   
boxplot(DATA$IPM,
        main = "Boxplot IPM",    # Judul Boxplot
        xlab = "IPM",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "lightblue")            # Warna kotak

boxplot(DATA$RLS,
        main = "Boxplot RLS",    # Judul Boxplot
        xlab = "RLS",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "pink")   
boxplot(DATA$jumlah.penduduk.miskin,
        main = "Boxplot Jumlah Penduduk Miskin",    # Judul Boxplot
        xlab = "Jumlah Penduduk Miskin",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "red")   
boxplot(DATA$UHH,
        main = "Boxplot UHH",    # Judul Boxplot
        xlab = "UHH",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "coral")
boxplot(DATA$Laju.PDRB,
        main = "Boxplot Laju PDRB",    # Judul Boxplot
        xlab = "Laju PDRB",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "chocolate")   
boxplot(DATA$jumlah.penduduk,
        main = "Boxplot Jumlah Penduduk",    # Judul Boxplot
        xlab = "Jumlah Penduduk ",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "yellow")   
boxplot(data3$jumlah.penduduk,
        main = "Boxplot Pengeluaran Perkapita",    # Judul Boxplot
        xlab = "Pengeluaran Perkapita",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "aquamarine")   
boxplot(DATA$Kepadatan.Penduduk,
        main = "Boxplot Kepadatan Penduduk",    # Judul Boxplot
        xlab = "Jumlah Kepadatan Penduduk",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "burlywood")   
boxplot(DATA$jumlah.bukan.angkatan.kerja,
        main = "Boxplot Penduduk Nonaktif",    # Judul Boxplot
        xlab = "Jumlah Penduduk Nonaktif",       # Label sumbu X
        ylab = "Nilai",               # Label sumbu Y
        col = "darkcyan")   
#regresi linier berganda
Model_OLS2<-lm(DATA$TPT~.,data = DATA)
Model_OLS<-lm(datatransform$ybin~.,data = datatransform)
summary(Model_OLS2)
rss <- sum(residuals(Model_OLS)^2)
n <- length(datatransform$ybin)  # jumlah observasi
p <- length(coefficients(Model_OLS))  # jumlah parameter dalam model, termasuk intercept
mse <- rss / (n - p)
mse
#uji t
p_value <- 2 * (1 - pt(abs(2.476), 144))
print(p_value)
#uji F
p_value <- 1 - pf(10.53, 9, 144)
print(p_value)
#cek multikolinearitas Agita
cor(DATA[,c(-1)])
vif(Model_OLS2) 
ols_vif_tol(Model_OLS2)
#Breusch Pagan test (heteroskedastisitas)
bptest(Model_OLS2, studentize=FALSE, data=DATA)
#uji normalitas dengan shapiro (Agita Andini)
residumodel<-resid(Model_OLS2)
jarque.bera.test(residumodel)


dataaa<-scale(DATA)
data3<-as.data.frame(dataaa)
summary(data3)
summary(datatransform)

#Transformasi Data#
Boxplot(xbin6)
boxplot(datatransform$X3)
x1_log <- log(DATA$IPM)
x2_log <- log(DATA$RLS)
x3_log <- log(DATA$jumlah.penduduk.miskin)
x4_log <- log(DATA$UHH)
x7_log <- log(DATA$Pengeluaran.perkapita)
x6_log <- log(DATA$jumlah.penduduk)
x8_log <- log(DATA$Kepadatan.Penduduk)
x9_log <- log(DATA$jumlah.bukan.angkatan.kerja)
#menggunakan data log
xbin1 = (1/sqrt(154-1))%*%(x1_log-mean(x1_log))/sd(x1_log)
xbin1
xbin2 = (1/sqrt(154-1))%*%(x2_log-mean(x2_log))/sd(x2_log)
xbin2
xbin3 = (1/sqrt(154-1))%*%(x3_log-mean(x3_log))/sd(x3_log)
xbin3
xbin4 = (1/sqrt(154-1))%*%(x4_log-mean(x4_log))/sd(x4_log)
xbin4
xbin5 = (1/sqrt(154-1))%*%(x51-mean(x51))/sd(x51)
xbin5
xbin6 = (1/sqrt(154-1))%*%(x61-mean(x61))/sd(x61)
xbin6
xbin6 = (1/sqrt(154-1))%*%(x6_log-mean(x6_log))/sd(x6_log)
xbin7 = (1/sqrt(154-1))%*%(x7_log-mean(x7_log))/sd(x7_log)
xbin7
xbin8 = (1/sqrt(154-1))%*%(x8_log-mean(x8_log))/sd(x8_log)
xbin8
xbin9 = (1/sqrt(154-1))%*%(x9_log-mean(x9_log))/sd(x9_log)
xbin9
ybin = matrix((1/sqrt(154-1))%*%(Y1-mean(Y1))/sd(Y1))
ybin=round(ybin,3)
xbin = matrix(c(xbin1,xbin2,xbin3,xbin4,xbin5,xbin6,xbin7,xbin8,xbin9), 154, 9)
xbin=round(xbin,3)
datatransform = data.frame(ybin,xbin)

xbin1 = (1/sqrt(154-1))%*%(x11-mean(x11))/sd(x11)
xbin1
xbin2 = (1/sqrt(154-1))%*%(x21-mean(x21))/sd(x21)
xbin2
xbin3 = (1/sqrt(154-1))%*%(x31-mean(x31))/sd(x31)
xbin3
xbin4 = (1/sqrt(154-1))%*%(x41-mean(x41))/sd(x41)
xbin4
xbin5 = (1/sqrt(154-1))%*%(x51-mean(x51))/sd(x51)
xbin5
xbin6 = (1/sqrt(154-1))%*%(x61-mean(x61))/sd(x61)
xbin6
xbin6 = (1/sqrt(154-1))%*%(x6_log-mean(x6_log))/sd(x6_log)
xbin7 = (1/sqrt(154-1))%*%(x71-mean(x71))/sd(x71)
xbin7
xbin8 = (1/sqrt(154-1))%*%(x81-mean(x81))/sd(x81)
xbin8
xbin9 = (1/sqrt(154-1))%*%(x91-mean(x91))/sd(x91)
xbin9
ybin = matrix((1/sqrt(154-1))%*%(Y1-mean(Y1))/sd(Y1))
ybin=round(ybin,3)
xbin = matrix(c(xbin1,xbin2,xbin3,xbin4,xbin5,xbin6,xbin7,xbin8,xbin9), 154, 9)
xbin=round(xbin,3)
datatransform = data.frame(ybin,xbin)

datatransform
dim(datatransform)
subset_data <- datatransform %>% slice(101:154)
#Mendeteksi Multikolinearitas dengan determinan matriks korelasi&nilai eigen
CC <- t(xbin)%*%xbin
CC
round(CC,2)
det(C)
e1 <- eigen(CC)
e1
eg1 <- matrix(e1$values)
eg1=(round(eg1,3))
round(eg1,4)

kondisi1 = eg1[1,]/eg1[9,] 
kondisi1

#Menduga parameter Generalized Ridge Regression#
C <- t(xbin)%*%xbin  
C
round(C,3)
D1 <- e1$vectors  #matriks C pada teori
D1
round(D1,3)
##Membuat matriks ortogonal
Z1 = xbin%*%D1
Z1
Z1=round(Z1,4)
z=data.frame(Z1)
subset_data <- z %>% slice(112:154)
lamda1 = t(Z1)%*%Z1
lamda1
round(lamda1,5)
#Menghitung nilai GAMMA OLS#
gamaOLS = solve(lamda1)%*%t(Z1)%*%ybin
gamaOLS
round(gamaOLS,3)

#Menghitung BETA OLS#
betaOLS = D1%*%gamaOLS
betaOLS
#MENENTUKAN TETAPAN BIAS K#
JKS = t(ybin)%*%ybin-t(gamaOLS)%*%t(Z1)%*%ybin
JKS
MSE = JKS/144 #coba pakai rumus n-k-1 (Kuadrat tengah residual)
MSE
round(MSE,2)

c1 = MSE/(gamaOLS[1,]^2)
c2 = MSE/(gamaOLS[2,]^2)
c3 = MSE/(gamaOLS[3,]^2)
c4 = MSE/(gamaOLS[4,]^2)
c5 = MSE/(gamaOLS[5,]^2)
c6 = MSE/(gamaOLS[6,]^2)
c7 = MSE/(gamaOLS[7,]^2)
c8 = MSE/(gamaOLS[8,]^2)
c9 = MSE/(gamaOLS[9,]^2)
K0 <- diag(c(c1,c2,c3,c4,c5,c6,c7,c8,c9))
k0=round(K0,4)

gamaGRR = (solve(lamda1+K0))%*%t(Z1)%*%ybin
gamaGRR
round(gamaGRR,4)
betaGRR = D1%*%gamaGRR
betaGRR
round(betaGRR,4)

JKSGRR = t(ybin)%*%ybin-t(gamaGRR)%*%t(Z1)%*%ybin
JKSGRR
MSEGRR = JKSGRR/144
MSEGRR
round(MSEGRR,4)

#Melakukan iterasi#
gamaGRa <- gamaOLS
gamaGRb <- gamaGRR
gamaGRR = solve(lamda1+K0)%*%t(Z1)%*%ybin
JKSGRR = t(ybin)%*%ybin-t(gamaGRR)%*%t(Z1)%*%ybin
MSEGRR = JKSGRR/144
i <- 0
err <- abs((t(gamaGRb)%*%gamaGRb)-
             (t(gamaGRa)%*%gamaGRa))
while (err >= 0.0001){
  i=i+1
  gamaGRa <- gamaGRb
  K0 =diag(c((MSEGRR/gamaGRb[1,]^2),(MSEGRR/gamaGRb[2,]^2),
           (MSEGRR/gamaGRb[3,]^2),(MSEGRR/gamaGRb[4,]^2),
           (MSEGRR/gamaGRb[5,]^2),(MSEGRR/gamaGRb[6,]^2),(MSEGRR/gamaGRb[7,]^2),
           (MSEGRR/gamaGRb[8,]^2),(MSEGRR/gamaGRb[9,]^2)))
  gamaGRb = solve(lamda1+K0)%*%t(Z1)%*%ybin
  JKSGRb = t(ybin)%*%ybin-t(gamaGRR)%*%(t(Z1)%*%ybin)
  MSEGRb = JKSGRR/144
  err <- abs((t(gamaGRb)%*%gamaGRb)- (t(gamaGRa)%*%gamaGRa))
  hasil = data.frame(K0,gamaGRb,err)
  print(hasil)
}

#APABILA TIDAK MEMENUHI LOOPING
k1 = MSEGRR/(gamaGRR[1,]^2)
k2 = MSEGRR/(gamaGRR[2,]^2)
k3 = MSEGRR/(gamaGRR[3,]^2)
k4 = MSEGRR/(gamaGRR[4,]^2)
k5 = MSEGRR/(gamaGRR[5,]^2)
k6 = MSEGRR/(gamaGRR[6,]^2)
k7 = MSEGRR/(gamaGRR[7,]^2)
k8 = MSEGRR/(gamaGRR[8,]^2)
k9 = MSEGRR/(gamaGRR[9,]^2)
K1 = diag(c(k1,k2,k3,k4,k5,k6,k7,k8,k9))
K1
round(K1,5)

gamaGR1 = solve(lamda1+K1)%*%t(Z1)%*%ybin
gamaGR1
round(gamaGR1,4)
#Menduga Parameter Jackknife Ridge Regression#
I = diag(c(1,1,1,1,1,1,1,1,1),9,9)
A = t(Z1)%*%Z1+K1
gamaJRR = (I-(solve(A)%*%K1)^2)%*%gamaOLS
round(gamaJRR,3)

BetaJRR = D1%*%gamaJRR
round(BetaJRR,3)
JKSJR = t(ybin)%*%ybin-t(gamaJRR)%*%(t(Z1)%*%ybin)
JKSJR
JKTJR = t(ybin)%*%ybin-154%*%mean(ybin)^2
JKTJR
JKRJR = t(gamaJRR)%*%(t(Z1)%*%ybin)-154%*%mean(ybin)^2
JKRJR
MSEJR = JKSJR/144 #kuadrat tengah residual
MSEJR
MSEJR2<-JKSJR/154
#Uji Koefisien Determinasi
koefdet = JKRJR/JKTJR
koefdet
Adjusted_R_squared = 1 - (1 - koefdet) * (153) / (144)
Adjusted_R_squared 
#Uji Simultan#
Fhit = (JKRJR/9)/(JKSJR/144)
Fhit
#Uji VIF
VIFJR =
  diag(solve(t(xbin)%*%xbin+K1)%*%(t(xbin)%*%xbin)%*%solve(t(xbin)%*%xbin+K1))
VIFJR
round(as.matrix(VIFJR),9)
#Uji Simultan#
Fhit = (JKRJR/9)/(JKSJR/144)
Fhit
#nilai Ftabel=1.96 (derajat bebas 0.05,9,113)
#Ragam Penduga Jackknife
mse <- diag((as.vector(MSEJR)),9,9)
mse
w = solve(A)
Q <- K1%*%w%*%K1
G <- I-w%*%Q
varjrr <- diag(G%*%mse%*%solve(lamda1)%*%t(G))
varjrr

vjr <- as.matrix(varjrr)
vjr
round(vjr,5)
#Uji Parsial
#Nilai Ttabel=1.98 (derajat bebas 0.05,113)
tj1 = BetaJRR[1,]/sqrt(vjr[1,])
tj1
tj2 = BetaJRR[2,]/sqrt(vjr[2,])
tj2
tj3 = BetaJRR[3,]/sqrt(vjr[3,])
tj3
tj4 = BetaJRR[4,]/sqrt(vjr[4,])
tj4
tj5 = BetaJRR[5,]/sqrt(vjr[5,])
tj5
tj6 = BetaJRR[6,]/sqrt(vjr[6,])
tj6
tj7 = BetaJRR[7,]/sqrt(vjr[7,])
tj7
tj8 = BetaJRR[8,]/sqrt(vjr[8,])
tj8
tj9 = BetaJRR[9,]/sqrt(vjr[9,])
tj9

#Transformasi data kebentuk awal dengan menggunakan seluruh variabel prediktor (METODE JRR)
bg1 = (sd(Y1)/sd(x11))%*%BetaJRR[1,]
round(bg1,5)
bg2 = (sd(Y1)/sd(x21))%*%BetaJRR[2,]
round(bg2,7)
bg3 = (sd(Y1)/sd(x31))%*%BetaJRR[3,]
round(bg3,7)
bg4 = (sd(Y1)/sd(x41))%*%BetaJRR[4,]
round(bg4,7)
bg5 = (sd(Y1)/sd(x51))%*%BetaJRR[5,]
round(bg5,7)
bg6 = (sd(Y1)/sd(x61))%*%BetaJRR[6,]
round(bg6,7)
bg7 = (sd(Y1)/sd(x71))%*%BetaJRR[7,]
round(bg7,7)
bg8 = (sd(Y1)/sd(x81))%*%BetaJRR[8,]
round(bg8,7)
bg9 = (sd(Y1)/sd(x91))%*%BetaJRR[9,]

round(bg9,7)
#Menentukan beta 0 metode jRR
b0_jrr = mean(Y1)-(bg1%*%mean(x11))-(bg2%*%mean(x21))-(bg3%*%mean(x31))-(bg4%*%mean(x41))-
  (bg5%*%mean(x51))-(bg6%*%mean(x61))-(bg7%*%mean(x71))-(bg8%*%mean(x81))-(bg9%*%mean(x91))
b0_jrr
b02 = mean(Y)-(bg1%*%mean(x1))-(bg2%*%mean(x2))-(bg3%*%mean(x3))-(bg4%*%mean(x4))-
  -(bg8%*%mean(x8))
b02
betaJRRbalik = c(b0,bg1,bg2,bg3,bg4,bg5,bg6,bg7,bg8,bg9)
betaJRRbalik
bjr <- as.matrix(betaJRRbalik)
bjr
round(bjr,8)
#menentukan nilai Y prediksi
predicted_test_jrr <-  b0_jrr+bg1*x11+bg2*x21+bg3*x31+bg4*x41+bg5*x51+bg6*x61+bg7*x71+bg8*x81+bg9*x91
  
  mse_test_jrr <- mean(residuals_testjrr^2)
  mse_test_jrr
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  #Menentukan Nilai Bias model#
  biasGRR = -D%*%(solve(A))%*%K1%*%t(D)%*%betaOLS
  round(biasGRR,4) 
  biasJRR = -D%*%((solve(A))%*%K1)^2%*%t(D)%*%betaOLS
  round(biasJRR,4)
  
  
#REGRESI RIDGE
  nilai_bias2<-lmridge(datatransform$ybin~.,data = datatransform)
  summary(nilai_bias2)
  kest(nilai_bias2)

  #estimasi regresi ridge dengan bias HKB
  modelll<-lmridge(datatransformmm$ybinn~., data = datatransformmm,K=c(0.07344))
  summary(modelll)
  vif(model1)
  
  #estimasi parameter ridge dengan bias LW (1976)
  model45<-lmridge(datatransformmm$ybinn~.,data = datatransformmm,K=c(0.09431))
  summary(model45)
  vif(model45)  
  