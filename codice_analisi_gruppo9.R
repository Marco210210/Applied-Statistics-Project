#setup

setwd('G:/Drive condivisi/Statistica/Statistica/R')
getwd()
data=read.csv('Dataset_AH_gruppo9.csv')
View(data)

#calcolo media mediana quartili prima di illustrarli graficamente

summary(data)



#istogrammi
par(mfrow=c(1,1))
hist(data$y_prestazSWcalc, ylab= 'Frequenza', xlab='prestazioneSWcalc' ,font=3, lwd=2, main= 'Distribuzione Prestazioni') 
hist(data$x1_CPU,xlab='CPU', ylab= 'Frequenza' ,font=3, lwd=2, main= 'Distribuzione CPU')
hist(data$x2_HD,xlab='HD' , ylab= 'Frequenza',font=3, lwd=2, main= 'Distribuzione HD')
hist(data$x3_proc,xlab='Proc' , ylab= 'Frequenza',font=3, lwd=2, main= 'Distribuzione Processi software')
hist(data$x4_aging,xlab='Anni' , ylab= 'Frequenza',font=3, lwd=2, main= 'Distribuzione Invecchiamento')
hist(data$x5_audio,xlab= 'Audio' , ylab= 'Frequenza',font=3, lwd=2, main= 'Distribuzione Audio')
hist(data$x6_RAM,xlab='RAM' , ylab= 'Frequenza',font=3, lwd=2, main= 'Distribuzione RAM')



#boxplot
boxplot(data$y_prestazSWcalc, main= 'BoxPlot Prestazioni')
boxplot(data$x1_CPU, main= 'BoxPlot CPU')
boxplot(data$x2_HD, main= 'BoxPlot HD')
boxplot(data$x3_proc, main= 'BoxPlot Processi software')
boxplot(data$x4_aging, main= 'BoxPlot Invecchiamento')
boxplot(data$x5_audio, main= 'BoxPlot Audio')
boxplot(data$x6_RAM, main= 'BoxPlot RAM')



#scatter plot e regressione polinomiale + indici di correlazione

#scatter plot Prestazione/CPU
plot(data$x1_CPU,data$y_prestazSWcalc, ylab= 'Prestazioni', xlab='CPU', main = 'Scatter plot Prestazioni/CPU')
myreg1=lm(y_prestazSWcalc ~ x1_CPU, data=data)
abline(myreg1)
myregpol1=lm(y_prestazSWcalc  ~ x1_CPU + I(x1_CPU^2),data=data)
lines(smooth.spline(data$x1_CPU,predict(myregpol1)),col="red",lwd=3)
summary(myreg1)
summary(myregpol1)
anova(myreg1,myregpol1)
cor(data$x1_CPU,data$y_prestazSWcalc)

#scatter plot Prestazione/HD
plot(data$x2_HD,data$y_prestazSWcalc, ylab= 'Prestazioni', xlab='HD', main = 'Scatter plot Prestazioni/HD')
myreg2=lm(y_prestazSWcalc ~ x2_HD, data=data)
abline(myreg2)
myregpol2=lm(y_prestazSWcalc  ~ x2_HD + I(x2_HD^2),data=data)
lines(smooth.spline(data$x2_HD,predict(myregpol2)),col="red",lwd=3)
summary(myreg2)
summary(myregpol2)
anova(myreg2,myregpol2)
cor(data$x2_HD,data$y_prestazSWcalc)#indice di correlazione

#scatter plot Prestazione/Proc
plot(data$x3_proc,data$y_prestazSWcalc, ylab= 'Prestazioni', xlab='Proc', main = 'Scatter plot Prestazioni/Proc')
myreg3=lm(y_prestazSWcalc ~ x3_proc, data=data)
abline(myreg3)
myregpol3=lm(y_prestazSWcalc  ~ x3_proc + I(x3_proc^2),data=data)
myregpol3_1=lm(y_prestazSWcalc  ~ x3_proc + I(x3_proc^2)+I(x3_proc^3),data=data)
lines(smooth.spline(data$x3_proc,predict(myregpol3)),col="red",lwd=3)
lines(smooth.spline(data$x3_proc,predict(myregpol3_1)),col="green",lwd=3)
summary(myreg3)
summary(myregpol3)
summary(myregpol3_1)
anova(myreg3,myregpol3)
anova(myregpol3_1,myregpol3)
cor(data$x3_proc,data$y_prestazSWcalc)#indice di correlazione

#scatter plot Prestazione/Aging
plot(data$x4_aging,data$y_prestazSWcalc, ylab= 'Prestazioni', xlab='Invecchiamento', main = 'Scatter plot Prestazioni/Invecchiamento')
myreg4=lm(y_prestazSWcalc ~ x4_aging, data=data)
abline(myreg4)
myregpol4=lm(y_prestazSWcalc  ~ x4_aging + I(x4_aging^2),data=data)
lines(smooth.spline(data$x4_aging,predict(myregpol4)),col="red",lwd=3)
summary(myreg4)
summary(myregpol4)
anova(myreg4,myregpol4)
cor(data$x4_aging,data$y_prestazSWcalc)#indice di correlazione

#scatter plot Prestazione/Audio
plot(data$x5_audio,data$y_prestazSWcalc, ylab= 'Prestazioni', xlab='Audio', main = 'Scatter plot Prestazioni/Audio')
myreg5=lm(y_prestazSWcalc ~ x5_audio, data=data)
abline(myreg5)
myregpol5=lm(y_prestazSWcalc  ~ x5_audio + I(x5_audio^2),data=data)
lines(smooth.spline(data$x5_audio,predict(myregpol5)),col="red",lwd=3)
summary(myreg5)
summary(myregpol5)
anova(myreg5,myregpol5)
cor(data$x5_audio,data$y_prestazSWcalc)#indice di correlazione

#scatter plot Prestazione/RAM
plot(data$x6_RAM,data$y_prestazSWcalc, ylab= 'Prestazioni', xlab='RAM',   main = 'Scatter plot Prestazioni/RAM')
myreg6=lm(y_prestazSWcalc ~ x6_RAM, data=data)
abline(myreg6,lwd=2)
myregpol6=lm(y_prestazSWcalc  ~ x6_RAM + I(x6_RAM^2),data=data)
lines(smooth.spline(data$x6_RAM,predict(myregpol6)),col="red",lwd=3)
summary(myreg6)
summary(myregpol6)
anova(myreg6,myregpol6)
cor(data$x6_RAM,data$y_prestazSWcalc)#indice di correlazione



#CORRPLOT DI TUTTE LE CORRELAZIONI TRA TUTTE LE VARIABILI DEL DATASET

#install.packages(corrplot)
library(corrplot)
cor = cor(data)
corrplot(cor,addCoef.col = "black")



#ANALISI DI REGRESSIONE
reg_lin1=lm(y_prestazSWcalc ~ ., data=data)
step_lin1=step(reg_lin1,direction="backward")
summary (step_lin1)
anova(reg_lin1, step_lin1)


reg_lin2=lm(y_prestazSWcalc ~ x1_CPU + x3_proc +I(x3_proc^2) + I(x3_proc^3) + x4_aging + x6_RAM,data=data)
step_lin2=step(reg_lin2, direction="backward")
summary (step_lin2)#modello migliore
anova(reg_lin2, step_lin2)



#INTERVALLI DI CONFIDENZA
confint(step_lin1)
confint(step_lin2)

#stima ai minimi quadrati
coef(step_lin1)
coef(step_lin2)



#analisi dei residui

#modello1
par(mfrow=c(1,2))
plot(step_lin1$residuals,ylab= 'Residui', xlab='Indice', main = 'Modello 1')
abline(0,0)
boxplot(step_lin1$residuals, main = 'Modello 1')
summary(step_lin1$residuals)

#modello 2
plot(step_lin2$residuals,ylab= 'Residui', xlab='Indice', main = 'Modello 2')
abline(0,0)
boxplot(step_lin2$residuals, main = 'Modello 2')
summary(step_lin2$residuals)


#grafici diagnostici
par(mfrow=c(2,2))
plot(step_lin1)
par(mfrow=c(2,2))
plot(step_lin2)

summary(step_lin1)
summary(step_lin2)
