getwd()
setwd("C:/Users/ignat/Downloads")
library(readxl)
library(dplyr)
library(nortest)
library(ggplot2)
data <- read_xlsx("Data Tuban Super Fix.xlsx", sheet = 4)


#DESKRIPTIF
prev_stunt_desk=aggregate(data$Prev_Stunt, list(data$Kecamatan), FUN = mean)
prev_stunt_desk[order(prev_stunt_desk$x),]

bblr_desk=aggregate(data$BBLR, list(data$Kecamatan), FUN = mean)
bblr_desk[order(bblr_desk$x),]

asi_desk=aggregate(data$ASI, list(data$Kecamatan), FUN = mean)
asi_desk[order(asi_desk$x),]

IDL_desk=aggregate(data$IDL, list(data$Kecamatan), FUN = mean)
IDL_desk[order(IDL_desk$x),]

PMTL_desk=aggregate(data$PMTLokal, list(data$Kecamatan), FUN = mean)
PMTL_desk[order(PMTL_desk$x),]

Im_Hepa_desk=aggregate(data$Im_Hepa_B, list(data$Kecamatan), FUN = mean)
Im_Hepa_desk[order(Im_Hepa_desk$x),]

Im_BCG_desk=aggregate(data$Im_BCG, list(data$Kecamatan), FUN = mean)
Im_BCG_desk[order(Im_BCG_desk$x),]

Im_Polio_desk=aggregate(data$Im_Polio, list(data$Kecamatan), FUN = mean)
Im_Polio_desk[order(Im_Polio_desk$x),]

Im_Campak_desk=aggregate(data$Im_Campak, list(data$Kecamatan), FUN = mean)
Im_Campak_desk[order(Im_Campak_desk$x),]

ggplot(data, aes(x = Prev_Stunt)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "orange", color = "black") +
  stat_density(geom = "line", aes(y = ..density..), color = "red", size = 1) +
  labs(x = "Prevalensi Stunting", y = "Kepadatan") +
  theme_minimal()

#Model Regresi Linear Berganda 1
regr.linear=lm(Prev_Stunt~BBLR+ASI+TTD+Im_Hepa_B+Im_Polio+Im_Campak, data=data)
summary(regr.linear)


#REGRESSION 2
regr.linear2=lm(Trans_PrevStunt~BBLR+ASI+TTD+Im_Hepa_B+Im_Polio+Im_Campak, data = data)
summary(regr.linear2)

#uji normalitas kolmogorov smirnov
res = regr.linear2$residual
ks.test(res,"pnorm",mean(res),sd(res),alternative=c("two.sided"))
ad.test(res)

#qqplot
qqnorm(res, pch = 1, frame = FALSE)
qqline(res, col = "red", lwd = 2)

#Boxplot Residual
boxplot(res, ylab = "residual")
boxplot.stats(res)$out

#uji moltikolinearitas (metode tolerans)
#VIF
library(olsrr)
ols_vif_tol(regr.linear)

#uji 
library(lmtest)
bptest(regr.linear) 
qchisq(1-0.1, 6) #Chisquare Table

#Uji Independen Residual
library(DescTools)
RunsTest(res)
dwtest(regr.linear2)

library(tmap)
library(raster)
library(spdep)
library(sf)
library(spatialreg)
library(geostan)

setwd("C:/Users/ignat/Downloads/00 Tuban/")
getwd()

spTuban=shapefile("Data_Stunting.shp")
names(spTuban)

#Menambahkan kolom variabel dalam file shp
spTuban$prev_stunt=data$Trans_PrevStunt
spTuban$pres_bblr=data$BBLR
spTuban$asi_eks=data$ASI
spTuban$vit_a=data$Vit_A
spTuban$air_bers=data$Air_Bersih
spTuban$TTD=data$TTD
spTuban$IDL=data$IDL
spTuban$PMT=data$PMTLokal
spTuban$Im_Hepa_B=data$Im_Hepa_B
spTuban$Im_Polio=data$Im_Polio
spTuban$Im_BCG=data$Im_BCG
spTuban$Im_DPT=data$Im_DPT
spTuban$Im_Campak=data$Im_Campak
spTuban$ISPA=data$ISPA

#MATRIKS PEMBOBOT QUEEN
queen.nb=poly2nb(spTuban) #Pembobot queen
queen.listw=nb2listw(queen.nb) #convert nb to listw type
queen.tuban= queen.listw

#MATRIKS PEMBOBOT Rook
rook.nb=poly2nb(spTuban, queen=FALSE) #Pembobot queen
rook.listw=nb2listw(rook.nb) #convert nb to listw type
rook.tuban= rook.listw

#Menyimpan Matriks Queen
bobot.queen = listw2mat(queen.listw) #convert listw to matrix
write.csv(bobot.queen, "Matriks Bobot Queen.csv")

setwd("C:/Users/ignat/Downloads")
getwd()

#MATRIKS PEMBOBOT Queen via GAL
queen.nb2=read.gal('Weight Tuban.gal') #Pembobot queen
queen.listw2=nb2listw(queen.nb2) #convert nb to listw type
queen.tuban2= queen.listw2

#Menyimpan Matriks Queen GAL
bobot.queen2 = listw2mat(queen.listw2) #convert listw to matrix
write.csv(bobot.queen2, "Matriks Bobot Queen2.csv")

#MATRIKS PEMBOBOT Rook via GAL
rook.nb2=read.gal('Rook_Tuban.gal') #Pembobot queen
rook.listw2=nb2listw(rook.nb2) #convert nb to listw type
rook.tuban2= rook.listw2

#Menyimpan Matriks Rook GAL
bobot.rook2 = listw2mat(rook.listw2) #convert listw to matrix
write.csv(bobot.rook2, "Matriks Bobot Rook2.csv")

#Checking Bobot Sama dengan GeoDa
isTRUE(all.equal(queen.nb,queen.nb2,check.attributes=FALSE))
isTRUE(all.equal(rook.nb,rook.nb2,check.attributes=FALSE))

#Moran Test: Pembobot Queen
moran_test=moran.test(res,queen.tuban, randomisation=TRUE, alternative = "greater")

# Nilai Moran's I yang diobservasi
I_observed <- moran_test$estimate[1]

# Nilai ekspektasi dari Moran's I di bawah hipotesis nol
I_expected <- moran_test$estimate[2]

# Variansi dari Moran's I
variance <- moran_test$estimate[3]

# Menghitung Z-score
Z_score <- (I_observed - I_expected) / sqrt(variance)
print(Z_score)
#=================================================================================
#Moran Test: Pembobot Rook
moran_test2=moran.test(res,rook.tuban, randomisation=TRUE, alternative = "greater")

# Nilai Moran's I yang diobservasi
I_observed2 <- moran_test2$estimate[1]

# Nilai ekspektasi dari Moran's I di bawah hipotesis nol
I_expected2 <- moran_test2$estimate[2]

# Variansi dari Moran's I
variance2 <- moran_test2$estimate[3]

# Menghitung Z-score
Z_score2 <- (I_observed2 - I_expected2) / sqrt(variance2)
print(Z_score2)
#==================================================================================

#Lagrange Multiplier
LM <- lm.LMtests(regr.linear2, queen.tuban,
                 test=c("LMlag","RLMlag","LMerr","RLMerr","SARMA"))
summary(LM)

#SAR Queen
reg.SAR=lagsarlm(prev_stunt~pres_bblr+asi_eks+TTD+Im_Hepa_B+Im_Polio+Im_Campak,
                 data=spTuban, queen.tuban)
summary(reg.SAR)

#SAR Rook
reg.SAR2=lagsarlm(prev_stunt~pres_bblr+asi_eks+TTD+Im_Hepa_B+Im_Polio+Im_Campak,
                 data=spTuban, rook.tuban)
summary(reg.SAR2)

#SEM Queen
reg.SEM=errorsarlm(prev_stunt~pres_bblr+asi_eks+TTD+Im_Hepa_B+Im_Polio+Im_Campak,
                   data=spTuban, queen.tuban)
summary(reg.SEM)

#SEM Rook
reg.SEM2=errorsarlm(prev_stunt~pres_bblr+asi_eks+TTD+Im_BCG+Im_Polio+Im_Campak,
                  data=spTuban, rook.tuban)
summary(reg.SEM2)

#SARMA
reg.SARMA=sacsarlm(prev_stunt~pres_bblr+asi_eks+TTD+Im_Hepa_B+Im_Polio+Im_Campak,
                   data=spTuban, queen.tuban)
reg.SARMA2=sacsarlm(prev_stunt~pres_bblr+asi_eks+Im_Hepa_B+Im_Polio+Im_Campak,
                    data=spTuban, queen.tuban)
reg.SARMA3=sacsarlm(prev_stunt~pres_bblr+asi_eks+Im_Hepa_B+Im_Campak,
                    data=spTuban, queen.tuban)
reg.SARMA4=sacsarlm(prev_stunt~pres_bblr+asi_eks+Im_Hepa_B,
                    data=spTuban, queen.tuban)

reg.SARMA5=sacsarlm(prev_stunt~pres_bblr+Im_Hepa_B,
                    data=spTuban, queen.tuban)

reg.SARMA_Rook=sacsarlm(prev_stunt~pres_bblr+asi_eks+TTD+Im_Hepa_B+Im_Polio+Im_Campak,
                   data=spTuban, rook.tuban)

reg.SARMA_Rook2=sacsarlm(prev_stunt~pres_bblr+asi_eks+Im_Hepa_B+Im_Polio+Im_Campak,
                        data=spTuban, rook.tuban)

reg.SARMA_Rook3=sacsarlm(prev_stunt~pres_bblr+asi_eks+Im_Hepa_B+Im_Campak,
                         data=spTuban, rook.tuban)

reg.SARMA_Rook4=sacsarlm(prev_stunt~pres_bblr+asi_eks+Im_Campak,
                         data=spTuban, rook.tuban)

reg.SARMA_Rook5=sacsarlm(prev_stunt~asi_eks+Im_Campak,
                         data=spTuban, rook.tuban)
summary(reg.SARMA)
summary(reg.SARMA2)
summary(reg.SARMA3)
summary(reg.SARMA4)
summary(reg.SARMA5)
summary(reg.SARMA_Rook)
summary(reg.SARMA_Rook2)
summary(reg.SARMA_Rook3)
summary(reg.SARMA_Rook4)
summary(reg.SARMA_Rook5)

AIC(regr.linear2)
AIC(reg.SAR)
AIC(reg.SEM)
AIC(reg.SARMA)
AIC(reg.SARMA5)
AIC(reg.SAR2)
AIC(reg.SEM2)
AIC(reg.SARMA_data2)
AIC(regr.linear2)

res.SARMA=reg.SARMA5$residuals
write.csv(res.SARMA, "Residual SARMA.csv")

# Nilai aktual dari variabel dependen
y_actual <- data$Trans_PrevStunt

# Nilai prediksi dari model
y_pred <- fitted.values(reg.SEM)

# Menghitung residual
residuals <- y_actual - y_pred

# Menghitung Total Sum of Squares (TSS)
y_mean <- mean(y_actual)
TSS <- sum((y_actual - y_mean)^2)

# Menghitung Residual Sum of Squares (RSS)
RSS <- sum(residuals^2)

# Menghitung R-squared
R2 <- 1 - (RSS / TSS)

print(paste("Nilai R^2:", R2))



# Log-likelihood dari model OLS
logLik_ols <- logLik(regr.linear2)

# Log-likelihood dari model SARMA
logLik_sarma <- logLik(reg.SARMA5)

# Likelihood Ratio Test antara model OLS dan SARMA
lr_test_sarma <- 2 * (logLik_sarma - logLik_ols)

# Menentukan p-value
p_value_sarma <- pchisq(lr_test_sarma, df = 1, lower.tail = FALSE)

#Moran's I SARMA
moran.test(res.SARMA,queen.tuban, randomisation=TRUE, alternative = "greater")

# Ekstraksi nilai rho dan beta dari model
rho <- reg.SARMA5$rho
beta <- reg.SARMA5$coefficients
lambda <- reg.SARMA5$lambda
# Hitung nilai u
I <- diag(length(data$Trans_PrevStunt))  # Matriks identitas
W1 <- as(as(queen.tuban, "CsparseMatrix"), "matrix")  # Konversi matriks bobot spasial menjadi matriks dense
W2 <- W1

# Variabel Dependen dan Independen
y <- data$Trans_PrevStunt
X <- model.matrix(~ BBLR+Im_Hepa_B, data = data)

# Hitung Nilai u dari Model Utama
u_main <- (I - rho * W1) %*% y - X %*% beta

# Memastikan konsistensi dengan persamaan gabungan
epsilon <- residuals(reg.SARMA5)
u_spatial <- solve(I - lambda * W2) %*% epsilon

u_spatial









































