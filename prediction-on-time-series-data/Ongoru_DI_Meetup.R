#Analizlerde kullanilacak paketler
paketler <- c("xlsx","dplyr","tidyr","forecast","expsmooth","lmtest","fma","tseries","fpp","xtable","ggplot2","opera","mixture")
#install.packages(paketler) #indirmek i�in
sapply(paketler, require, character.only = T) #y�klemek i�in

#Turizm verilerine ulasalim (dosya adindaki yazim hatasina dikkat!)
temp <- tempfile()
download.file(url="https://www.tursab.org.tr/dosya/69/tursit-sayisi695509202-1695597978-1695423595-2_69_5856915.xls",temp,mode="wb")
hamveri0<-read.xlsx(temp,1)
#hamveri0<-read.csv(file="C:/users/eren/desktop/turist_sayisi.csv",sep=",",header=T) #alternatif olarak


#Verileri analize hazir hale getirelim
hamveri1<-slice(hamveri0,4:15)
hamveri2<-hamveri1 %>% gather(aylar,turist,NA.:NA..27)
#hamveri2<-hamveri1 %>% gather(aylar,turist,X:X.27) #eger csv uzantili dosya kullanilacaksa
hamveri3<-hamveri2[3]
hamveri4<-lapply(hamveri3,function(x){as.numeric(gsub(" ", "", gsub(",","",x)))})
veri<-ts(as.numeric(unlist(hamveri4)), start=1990, frequency=12)

#Tahmin ve test �rneklemlerini ayiralim
train<-window(veri,start=c(1990,1),end=c(2014,12))
test<-window(veri,start=c(2015,1),end=c(2017,6))
horizon<-length(test) #�ng�r� ufku

#Bazi betimleyici istatistikleri alalim
head(train)
tail(train)
autoplot(train)
ggmonthplot(train)
summary(train)
hist(train)
boxplot(train)
#jarque.bera.test(train) #�ok lazimsa (ki degil), normallik testi de yapabiliriz

#Verileri d�n�st�rmeli miyiz? (y^lmd - 1 / lmd, log(y) d�n�st�rmesi i�in lmd=0)
lmd<-BoxCox.lambda(train,method=c("guerrero","loglik"),lower=-1,upper=2) #alt k�melerde degiskenlik katsayisini enk���kl�yor
autoplot(BoxCox(train,lambda=lmd)) #d�n�st�rmeye karar verseydik
#InvBoxCox(y,lambda) #d�n�st�rme islemini geri almak i�in


#Geleneksel ayristirma modeli (artik pek tercih edilmiyor. sadece bilesenler hakkinda kabaca fikir edinmek i�in)
add_decomp_model <- decompose(train, type="additive")
mul_decomp_model <- decompose(train, type="multiplicative")
autoplot(add_decomp_model)
autoplot(mul_decomp_model)

#STL ayristirma y�ntemi (her frekansta �alisir, u� g�zlemlere karsi dayaniklidir ve belli kaliplari yoktur)
stl_model<-stl(log(train),t.window=15,s.window=5,robust=TRUE) #farkli girdi parametreleri de denenmeli
plot(stl_model)
#plot(stl_model$time.series[,"seasonal"], main="", ylab="Mevsimsel") #tekil bilesenler i�in (�rn. mevsimsellik)
plot(log(train), col="gray",
     main="Turizm",
     ylab="Turist sayisi (logaritmik)", xlab="")
lines(stl_model$time.series[,2],col="red",ylab="Egilim")

plot(train, col="grey",
     main="Turizm",
     xlab="", ylab="Turist sayisi")
lines(exp(seasadj(stl_model)),col="red",ylab="Mevsimsel d�zeltilmis")

autoplot(forecast(stl_model,h=horizon,method="naive"),include=80) #model �ng�r�lerini g�rsellestirelim

#�stel d�zeltme ayristirma y�ntemi (ETS'nin �zel halleri olarak modellenebiliyor)
add_expsm_model <- hw(train,seasonal="additive",lambda=1,h=horizon)
mul_expsm_model <- hw(train,seasonal="multiplicative",h=horizon)

plot(train,ylab="Turist sayisi (kisi)",
     plot.conf=FALSE, type="l", fcol="white", xlab="Yil")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt-Winters' Additive","Holt-Winters' Multiplicative"))
lines(fitted(add_expsm_model), col="red", lty=1)
lines(fitted(mul_expsm_model), col="green", lty=1)
lines(add_expsm_model$mean, type="o", col="red")
lines(mul_expsm_model$mean, type="o", col="green")

bilesenler <- cbind(add_expsm_model$model$states[,1:3],mul_expsm_model$model$states[,1:3])
colnames(bilesenler) <- c("seviye","egim","mevsim","seviye","egim","mevsim")
plot(bilesenler, xlab="Yil")
add_expsm_model$model$state[,1:3]
fitted(add_expsm_model)
add_expsm_model$mean

#Error-Trend-Seasonality (ETS) ayristirma y�ntemi (duragan ve/veya dogrusal "olmayan" verilerde basarimi y�ksek)
#ets_model <- ets(train, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL, 
#                 gamma=NULL, phi=NULL, additive.only=FALSE, lambda=0,
#                 lower=c(rep(0.0001,3), 0.8), upper=c(rep(0.9999,3),0.98),
#                 opt.crit="lik", nmse=3, bounds="both", ic="aicc", restrict=TRUE)
ets_model <- ets(train,model="ZZZ",lambda=0)
summary(ets_model)
autoplot(ets_model)
autoplot(forecast(ets_model, h=horizon), ylab="Turist sayisi (kisi)",include=80)
#plot(forecast(ets_model, h=horizon,level=c(80,95), fan=TRUE, simulate=TRUE, bootstrap=TRUE, npaths=5000, PI=TRUE, lambda=0)) #benzetim ile �ng�r�

#Duraganligi nasil sinayacagiz? (u� g�zlemlere ve yapisal kirilmalara dikkat!)
adf.test(train, alternative = "stationary") #sifir hipotezi: birim k�k vardir (sabit terim ve egilim dahil)
kpss.test(train,null="Trend") #sifir hipotezi: birim k�k yoktur (sabit terim ve egilim dahil)
par(mfrow=c(1,2))
Acf(train,lag.max=36,main="Autocorrelation")
Pacf(train,lag.max=36,main="Partial Autocorrelation")

#Autoregressive Integrated Moving Average (ARIMA) modelleme y�ntemi (kisa d�nemli tahmin basarimi y�ksek)
#auto.arima(train,stepwise=FALSE,approximation=FALSE,seasonal=TRUE,trace=FALSE,parallel=TRUE,num.cores=4,lambda=0) #girdi arg�manlari
arima_model <- auto.arima(train,lambda=0) 
summary(arima_model)
ggtsdisplay(residuals(arima_model))
Box.test(residuals(arima_model), lag=36,type="Ljung")
#checkresiduals(arima_model,lag=36,test="LB") #daha genel ama�li bir alternatif olarak kullanilabilir
autoplot(forecast(arima_model,h=horizon),include=80)
#auto.arima(log(diff(train,12)),stepwise=FALSE,approximation=FALSE,seasonal=TRUE) #bagimli degiskeni d�n�st�rerek devam edebiliriz

#Autoregressive Neural Network modelleme y�ntemi (ileri beslemeli ve tek gizli katmanli)
#nnetar(veri,p=5,P=1,size=3,repeats=20,xreg=NULL,lambda=NULL,model=NULL,subset=NULL,scale.inputs=TRUE) #girdi arg�manlari
neural_model <- nnetar(train,lambda=0) #NNAR(p,P,k)[m] aslinda ARIMA(p,0,0)(P,0,0)[m] modelinin dogrusal olmayan karsiligi gibi
autoplot(forecast(neural_model,PI=TRUE,npaths=1000,h=horizon),include=80)
accuracy(fitted(neural_model),train) #summary fonksiyonu ile basarim �l��tlerini alamiyoruz
neural_model_test <- nnetar(test,model=neural_model) #Ayni model kalibini test k�mesine de uygulayalim
accuracy(fitted(neural_model_test),test) 
par(mfrow=c(1,1))
plot(veri, xlim=c(2010,2018))
lines(fitted(neural_model_test), col="red", lty=1)

#Tahmin basarimlarinin karsilastirilmasi (istatistiki a�idan iyi ve ge�erli bir model, basarili bir �ng�r� yapmak zorunda degil! NEDEN?)
#criteria<-names(accuracy(forecast(ets_model,h=horizon),test)[2,]) #�nceki bir modelden �l��t isimlerini alalim
criteria<-c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")
feval1<-accuracy(forecast(ets_model,h=horizon),test)[2,criteria]
feval2<-accuracy(forecast(arima_model,h=horizon),test)[2,criteria]
feval3<-accuracy(forecast(neural_model,h=horizon),test)[2,criteria]

compmat <- matrix(c(feval1,feval2,feval3),nrow=3,ncol=length(criteria),byrow=TRUE)
colnames(compmat) <- criteria
rownames(compmat) <- c("ETS","ARIMA","NNET")
compmat
#xtable(compmat,auto.format=TRUE) #latex tablosu i�in

#Mevcut �ng�r�leri g�rsel olarak karsilastiralim
models <- cbind(ETS=forecast(ets_model,h=horizon)$mean, 
                ARIMA=forecast(arima_model,h=horizon)$mean, 
                NNET=forecast(neural_model,h=horizon)$mean)
veri_fcast <- cbind(veri, models)
colnames(veri_fcast) <- c("Data","ETS","ARIMA","NNET")
autoplot(veri_fcast) + xlab("Year") + ylab("Turist Sayisi") + xlim(c(2010,2019))

#Mevcut �ng�r�leri birlestirelim ve tek patikaya indirgeyelim
model_comb<-mixture(model="MLpol",loss.type="square")
comb_weights<-predict(model_comb,models,test,type='weights')
fcast_comb <- ts(predict(model_comb, models, test, type='response'), start=c(2015,1), freq=12)
#accuracy(fcast_comb,test)
veri_fcomb <- cbind(veri, fcast_comb)
colnames(veri_fcomb) <- c("Data","Combined")
autoplot(veri_fcomb) + xlab("Year") + ylab("Turist Sayisi") + xlim(c(2010,2019))

#Modelleri g�zden ge�irip nihai hallerini verelim (modeli train+test k�mesi i�in tekrar �alistirmak yeterli, ama...)
hend<-18 #�ng�r� ufkunu 2018 yili sonu olarak belirleyelim 
sifir<-ts(0,frequency=12,start=c(1990,1),end=c(2018,12)) #�ng�r� ufkunun sonuna kadar bir zaman serisi yaratalim
change<-ifelse(time(sifir)>=2015+11/12,1,0) #kirilmayi bir kukla degisken ile tanimlayalim
arimax_model <- auto.arima(na.omit(veri),xreg=window(change,end=c(2017,6)),lambda=0) #ARIMA modelimize a�iklayici degisken olarak ilave edelim
etsf_model <- ets(na.omit(veri),model="ZZZ",lambda=0)
neuralf_model <- nnetar(na.omit(veri),lambda=0)
autoplot(forecast(arimax_model,xreg=window(change,start=c(2017,7)),h=hend)) + xlim(c(2010,2019))
autoplot(forecast(etsf_model,h=hend)) + xlim(c(2010,2019))
autoplot(forecast(neuralf_model,PI=TRUE,npaths=1000,h=hend)) + xlim(c(2010,2019))

#Son olarak bu �� modelin �ng�r�leri de birlestirilebilir...