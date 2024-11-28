#####graficos p=3 2 misturas



####boxplot
#########################################
#######       nu=0.2            #########
#########################################
#alpha1,2,3

alpha1n100=NULL
alpha2n100=NULL
alpha3n100=NULL
alpha1n200=NULL
alpha2n200=NULL
alpha3n200=NULL
alpha1n400=NULL
alpha2n400=NULL
alpha3n400=NULL
alpha1n800=NULL
alpha2n800=NULL
alpha3n800=NULL
alpha1n1000=NULL
alpha2n1000=NULL
alpha3n1000=NULL
beta1n100=NULL
beta2n100=NULL
beta3n100=NULL
beta1n200=NULL
beta2n200=NULL
beta3n200=NULL
beta1n400=NULL
beta2n400=NULL
beta3n400=NULL
beta1n800=NULL
beta2n800=NULL
beta3n800=NULL
beta1n1000=NULL
beta2n1000=NULL
beta3n1000=NULL
sigma11n100=NULL
sigma12n100=NULL
sigma13n100=NULL
sigma22n100=NULL
sigma23n100=NULL
sigma33n100=NULL
sigma11n200=NULL
sigma12n200=NULL
sigma13n200=NULL
sigma22n200=NULL
sigma23n200=NULL
sigma33n200=NULL
sigma11n400=NULL
sigma12n400=NULL
sigma13n400=NULL
sigma22n400=NULL
sigma23n400=NULL
sigma33n400=NULL
sigma11n800=NULL
sigma12n800=NULL
sigma13n800=NULL
sigma22n800=NULL
sigma23n800=NULL
sigma33n800=NULL
sigma11n1000=NULL
sigma12n1000=NULL
sigma13n1000=NULL
sigma22n1000=NULL
sigma23n1000=NULL
sigma33n1000=NULL
nu1n100=NULL
nu2n100=NULL
nu1n200=NULL
nu2n200=NULL
nu1n400=NULL
nu2n400=NULL
nu1n800=NULL
nu2n800=NULL
nu1n1000=NULL
nu2n1000=NULL
pii1n100=NULL
pii2n100=NULL
pii1n200=NULL
pii2n200=NULL
pii1n400=NULL
pii2n400=NULL
pii1n800=NULL
pii2n800=NULL
pii1n1000=NULL
pii2n1000=NULL
for (i in 1:1000) {
  datosn100<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon100.2misturas.RData")
  
  alpha1n100[i]=resultado[[i]]$alpha[1,]
  alpha2n100[i]=resultado[[i]]$alpha[2,]
  alpha3n100[i]=resultado[[i]]$alpha[3,]
  beta1n100[i]=resultado[[i]]$beta[1,]
  beta2n100[i]=resultado[[i]]$beta[2,]
  beta3n100[i]=resultado[[i]]$beta[3,]
  sigma11n100[i]=resultado[[i]]$Sigma[1,1]
  sigma12n100[i]=resultado[[i]]$Sigma[1,2]
  sigma13n100[i]=resultado[[i]]$Sigma[1,3]
  sigma22n100[i]=resultado[[i]]$Sigma[2,2]
  sigma23n100[i]=resultado[[i]]$Sigma[2,3]
  sigma33n100[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2]))
  nu1n100[i]=orden[1]
  nu2n100[i]=orden[2]
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2]))
  pii1n100[i]=orden[1]
  pii2n100[i]=orden[2]
  datosn200<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon200.2misturas.RData")
  alpha1n200[i]=resultado[[i]]$alpha[1,]
  alpha2n200[i]=resultado[[i]]$alpha[2,]
  alpha3n200[i]=resultado[[i]]$alpha[3,]
  beta1n200[i]=resultado[[i]]$beta[1,]
  beta2n200[i]=resultado[[i]]$beta[2,]
  beta3n200[i]=resultado[[i]]$beta[3,]
  sigma11n200[i]=resultado[[i]]$Sigma[1,1]
  sigma12n200[i]=resultado[[i]]$Sigma[1,2]
  sigma13n200[i]=resultado[[i]]$Sigma[1,3]
  sigma22n200[i]=resultado[[i]]$Sigma[2,2]
  sigma23n200[i]=resultado[[i]]$Sigma[2,3]
  sigma33n200[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2]))
  nu1n200[i]=orden[1]
  nu2n200[i]=orden[2]
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2]))
  pii1n200[i]=orden[1]
  pii2n200[i]=orden[2]
  datosn400<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon400.2misturas.RData")
  alpha1n400[i]=resultado[[i]]$alpha[1,]
  alpha2n400[i]=resultado[[i]]$alpha[2,]
  alpha3n400[i]=resultado[[i]]$alpha[3,]
  beta1n400[i]=resultado[[i]]$beta[1,]
  beta2n400[i]=resultado[[i]]$beta[2,]
  beta3n400[i]=resultado[[i]]$beta[3,]
  sigma11n400[i]=resultado[[i]]$Sigma[1,1]
  sigma12n400[i]=resultado[[i]]$Sigma[1,2]
  sigma13n400[i]=resultado[[i]]$Sigma[1,3]
  sigma22n400[i]=resultado[[i]]$Sigma[2,2]
  sigma23n400[i]=resultado[[i]]$Sigma[2,3]
  sigma33n400[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2]))
  nu1n400[i]=orden[1]
  nu2n400[i]=orden[2]
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2]))
  pii1n400[i]=orden[1]
  pii2n400[i]=orden[2]
  datosn800<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon800.2misturas.RData")
  alpha1n800[i]=resultado[[i]]$alpha[1,]
  alpha2n800[i]=resultado[[i]]$alpha[2,]
  alpha3n800[i]=resultado[[i]]$alpha[3,]
  beta1n800[i]=resultado[[i]]$beta[1,]
  beta2n800[i]=resultado[[i]]$beta[2,]
  beta3n800[i]=resultado[[i]]$beta[3,]
  sigma11n800[i]=resultado[[i]]$Sigma[1,1]
  sigma12n800[i]=resultado[[i]]$Sigma[1,2]
  sigma13n800[i]=resultado[[i]]$Sigma[1,3]
  sigma22n800[i]=resultado[[i]]$Sigma[2,2]
  sigma23n800[i]=resultado[[i]]$Sigma[2,3]
  sigma33n800[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2]))
  nu1n800[i]=orden[1]
  nu2n800[i]=orden[2]
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2]))
  pii1n800[i]=orden[1]
  pii2n800[i]=orden[2]
  datosn1000<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon1000.2misturas.RData")
  alpha1n1000[i]=resultado[[i]]$alpha[1,]
  alpha2n1000[i]=resultado[[i]]$alpha[2,]
  alpha3n1000[i]=resultado[[i]]$alpha[3,]
  beta1n1000[i]=resultado[[i]]$beta[1,]
  beta2n1000[i]=resultado[[i]]$beta[2,]
  beta3n1000[i]=resultado[[i]]$beta[3,]
  sigma11n1000[i]=resultado[[i]]$Sigma[1,1]
  sigma12n1000[i]=resultado[[i]]$Sigma[1,2]
  sigma13n1000[i]=resultado[[i]]$Sigma[1,3]
  sigma22n1000[i]=resultado[[i]]$Sigma[2,2]
  sigma23n1000[i]=resultado[[i]]$Sigma[2,3]
  sigma33n1000[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2]))
  nu1n1000[i]=orden[1]
  nu2n1000[i]=orden[2]
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2]))
  pii1n1000[i]=orden[1]
  pii2n1000[i]=orden[2]
}
par(mfrow=c(1,1))
alpha1<-data.frame(n100=alpha1n100,n200=alpha1n200,n400=alpha1n400,n800=alpha1n800,n1000=alpha1n1000)
boxplot(alpha1,col =grey.colors(5, start = 0.2, end = 1),pch=16,ylim=c(-0.5,0.5)
        ,main=expression(paste( alpha[1] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=0,b=0)
#legend("bottom", inset=.02, c("100","200","400","800","100"), fill=grey.colors(4, start = 0.2, end = 1), cex=1.2)
legend("bottom", inset=0.01, legend=c("100", "200", "400", "800", "1000"),
       fill=grey.colors(5, start=0.2, end=1), horiz=TRUE, cex=1, x.intersp=0.5)

alpha2<-data.frame(n100=alpha2n100,n200=alpha2n200,n400=alpha2n400,n800=alpha2n800,n1000=alpha2n1000)
boxplot(alpha2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( alpha[2] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=-0.2,b=0)
alpha3<-data.frame(n100=alpha3n100,n200=alpha3n200,n400=alpha3n400,n800=alpha3n800,n1000=alpha3n1000)
boxplot(alpha3,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( alpha[3] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=0.3,b=0)
#######beta
beta1<-data.frame(n100=beta1n100,n200=beta1n200,n400=beta1n400,n800=beta1n800,n1000=beta1n1000)
boxplot(beta1,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( beta[1] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
#legend("bottom", inset=.02, c("100","200","400","800","100"), fill=grey.colors(4, start = 0.2, end = 1), cex=1.2)
#legend("bottom", inset=-0.01, legend=c("100", "200", "400", "800", "1000"),
#       fill=grey.colors(5, start=0.2, end=0.8), horiz=TRUE, cex=1, x.intersp=0.5)

beta2<-data.frame(n100=beta2n100,n200=beta2n200,n400=beta2n400,n800=beta2n800,n1000=beta2n1000)
boxplot(beta2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( beta[2] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1.5,b=0)
beta3<-data.frame(n100=beta3n100,n200=beta3n200,n400=beta3n400,n800=beta3n800,n1000=beta3n1000)
boxplot(beta3,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( beta[3] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=0.6,b=0)
#####sigma
sigma11<-data.frame(n100=sigma11n100,n200=sigma11n200,n400=sigma11n400,n800=sigma11n800,n1000=sigma11n1000)
boxplot(beta1,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[11] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
#legend("bottom", inset=.02, c("100","200","400","800","100"), fill=grey.colors(4, start = 0.2, end = 1), cex=1.2)
#legend("bottom", inset=-0.01, legend=c("100", "200", "400", "800", "1000"),
#       fill=grey.colors(5, start=0.2, end=0.8), horiz=TRUE, cex=1, x.intersp=0.5)
sigma12<-data.frame(n100=sigma12n100,n200=sigma12n200,n400=sigma12n400,n800=sigma12n800,n1000=sigma12n1000)
boxplot(sigma12,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[12] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
sigma13<-data.frame(n100=sigma13n100,n200=sigma13n200,n400=sigma13n400,n800=sigma13n800,n1000=sigma13n1000)
boxplot(sigma13,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[13] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
sigma22<-data.frame(n100=sigma22n100,n200=sigma22n200,n400=sigma22n400,n800=sigma22n800,n1000=sigma22n1000)
boxplot(sigma22,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[22] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=2.5,b=0)
sigma23<-data.frame(n100=sigma23n100,n200=sigma23n200,n400=sigma23n400,n800=sigma23n800,n1000=sigma23n1000)
boxplot(sigma23,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[23] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
sigma33<-data.frame(n100=sigma33n100,n200=sigma33n200,n400=sigma33n400,n800=sigma33n800,n1000=sigma33n1000)
boxplot(sigma33,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[33] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=5,b=0)
#####################nu
nu1<-data.frame(n100=nu1n100,n200=nu1n200,n400=nu1n400,n800=nu1n800,n1000=nu1n1000)
boxplot(nu1,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( psi[1] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
#legend("bottom", inset=.02, c("100","200","400","800","100"), fill=grey.colors(4, start = 0.2, end = 1), cex=1.2)
#legend("bottom", inset=-0.01, legend=c("100", "200", "400", "800", "1000"),
#       fill=grey.colors(5, start=0.2, end=0.8), horiz=TRUE, cex=1, x.intersp=0.5)

nu2<-data.frame(n100=nu2n100,n200=nu2n200,n400=nu2n400,n800=nu2n800,n1000=nu2n1000)
boxplot(nu2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( psi[2] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=2,b=0)
#######pii
pii1<-data.frame(n100=pii1n100,n200=pii1n200,n400=pii1n400,n800=pii1n800,n1000=pii1n1000)
boxplot(pii1,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( p[1] )),names = F,cex.main=1.5,cex.axis=1.5,ylim=c(-0.4,1))
abline(a=0.3,b=0)
pii2<-data.frame(n100=pii2n100,n200=pii2n200,n400=pii2n400,n800=pii2n800,n1000=pii2n1000)
boxplot(pii2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( p[2] )),names = F,cex.main=1.5,cex.axis=1.5,ylim=c(0,1.2))
abline(a=0.7,b=0)
fila0<-(cbind(mean(nu1$n200),median(nu1$n200),sd(nu1$n200),mean(nu1$n400),median(nu1$n400),sd(nu1$n400)))
fila0.1<-(cbind(mean(nu2$n200),median(nu2$n200),sd(nu2$n200),mean(nu2$n400),median(nu2$n400),sd(nu2$n400)))
fila1<-(cbind(mean(alpha1$n200),median(alpha1$n200),sd(alpha1$n200),mean(alpha1$n400),median(alpha1$n400),sd(alpha1$n400)))
fila2<-(cbind(mean(alpha2$n200),median(alpha2$n200),sd(alpha2$n200),mean(alpha2$n400),median(alpha2$n400),sd(alpha2$n400)))
fila3<-(cbind(mean(alpha3$n200),median(alpha3$n200),sd(alpha3$n200),mean(alpha3$n400),median(alpha3$n400),sd(alpha3$n400)))
fila4<-(cbind(mean(beta1$n200),median(beta1$n200),sd(beta1$n200),mean(beta1$n400),median(beta1$n400),sd(beta1$n400)))
fila5<-(cbind(mean(beta2$n200),median(beta2$n200),sd(beta2$n200),mean(beta2$n400),median(beta2$n400),sd(beta2$n400)))
fila6<-(cbind(mean(beta3$n200),median(beta3$n200),sd(beta3$n200),mean(beta3$n400),median(beta3$n400),sd(beta3$n400)))
fila7<-(cbind(mean(sigma11$n200),median(sigma11$n200),sd(sigma11$n200),mean(sigma11$n400),median(sigma11$n400),sd(sigma11$n400)))
fila8<-(cbind(mean(sigma12$n200),median(sigma12$n200),sd(sigma12$n200),mean(sigma12$n400),median(sigma12$n400),sd(sigma12$n400)))
fila9<-(cbind(mean(sigma13$n200),median(sigma13$n200),sd(sigma13$n200),mean(sigma13$n400),median(sigma13$n400),sd(sigma13$n400)))
fila10<-(cbind(mean(sigma22$n200),median(sigma22$n200),sd(sigma22$n200),mean(sigma22$n400),median(sigma22$n400),sd(sigma22$n400)))
fila11<-(cbind(mean(sigma23$n200),median(sigma23$n200),sd(sigma23$n200),mean(sigma23$n400),median(sigma23$n400),sd(sigma23$n400)))
fila12<-(cbind(mean(sigma33$n200),median(sigma33$n200),sd(sigma33$n200),mean(sigma33$n400),median(sigma33$n400),sd(sigma33$n400)))
fila13<-(cbind(mean(pii1$n200),median(pii1$n200),sd(pii1$n200),mean(pii1$n400),median(pii1$n400),sd(pii1$n400)))
fila14<-(cbind(mean(pii2$n200),median(pii2$n200),sd(pii2$n200),mean(pii2$n400),median(pii2$n400),sd(pii2$n400)))
parte1 <- rbind(fila0,fila0.1,fila1, fila2, fila3, fila4, fila5, fila6, fila7, fila8, fila9, fila10, fila11, fila12, fila13, fila14)
fila0<-(cbind(mean(nu1$n800),median(nu1$n800),sd(nu1$n800),mean(nu1$n1000),median(nu1$n1000),sd(nu1$n1000)))
fila0.1<-(cbind(mean(nu2$n800),median(nu2$n800),sd(nu2$n800),mean(nu2$n1000),median(nu2$n1000),sd(nu2$n1000)))
fila1<-(cbind(mean(alpha1$n800),median(alpha1$n800),sd(alpha1$n800),mean(alpha1$n1000),median(alpha1$n1000),sd(alpha1$n1000)))
fila2<-(cbind(mean(alpha2$n800),median(alpha2$n800),sd(alpha2$n800),mean(alpha2$n1000),median(alpha2$n1000),sd(alpha2$n1000)))
fila3<-(cbind(mean(alpha3$n800),median(alpha3$n800),sd(alpha3$n800),mean(alpha3$n1000),median(alpha3$n1000),sd(alpha3$n1000)))
fila4<-(cbind(mean(beta1$n800),median(beta1$n800),sd(beta1$n800),mean(beta1$n1000),median(beta1$n1000),sd(beta1$n1000)))
fila5<-(cbind(mean(beta2$n800),median(beta2$n800),sd(beta2$n800),mean(beta2$n1000),median(beta2$n1000),sd(beta2$n1000)))
fila6<-(cbind(mean(beta3$n800),median(beta3$n800),sd(beta3$n800),mean(beta3$n1000),median(beta3$n1000),sd(beta3$n1000)))
fila7<-(cbind(mean(sigma11$n800),median(sigma11$n800),sd(sigma11$n800),mean(sigma11$n1000),median(sigma11$n1000),sd(sigma11$n1000)))
fila8<-(cbind(mean(sigma12$n800),median(sigma12$n800),sd(sigma12$n800),mean(sigma12$n1000),median(sigma12$n1000),sd(sigma12$n1000)))
fila9<-(cbind(mean(sigma13$n800),median(sigma13$n800),sd(sigma13$n800),mean(sigma13$n1000),median(sigma13$n1000),sd(sigma13$n1000)))
fila10<-(cbind(mean(sigma22$n800),median(sigma22$n800),sd(sigma22$n800),mean(sigma22$n1000),median(sigma22$n1000),sd(sigma22$n1000)))
fila11<-(cbind(mean(sigma23$n800),median(sigma23$n800),sd(sigma23$n800),mean(sigma23$n1000),median(sigma23$n1000),sd(sigma23$n1000)))
fila12<-(cbind(mean(sigma33$n800),median(sigma33$n800),sd(sigma33$n800),mean(sigma33$n1000),median(sigma33$n1000),sd(sigma33$n1000)))
fila13<-(cbind(mean(pii1$n800),median(pii1$n800),sd(pii1$n800),mean(pii1$n1000),median(pii1$n1000),sd(pii1$n1000)))
fila14<-(cbind(mean(pii2$n800),median(pii2$n800),sd(pii2$n800),mean(pii2$n1000),median(pii2$n1000),sd(pii2$n1000)))
parte2 <- rbind(fila0,fila0.1,fila1, fila2, fila3, fila4, fila5, fila6, fila7, fila8, fila9, fila10, fila11, fila12, fila13, fila14)

tabla<-rbind(parte1,parte2)
xtable::xtable(tabla,digits=4)
####boxplot
############################################
#######      3 misturas            #########
############################################


alpha1n100=NULL
alpha2n100=NULL
alpha3n100=NULL
alpha1n200=NULL
alpha2n200=NULL
alpha3n200=NULL
alpha1n400=NULL
alpha2n400=NULL
alpha3n400=NULL
alpha1n800=NULL
alpha2n800=NULL
alpha3n800=NULL
alpha1n1000=NULL
alpha2n1000=NULL
alpha3n1000=NULL
beta1n100=NULL
beta2n100=NULL
beta3n100=NULL
beta1n200=NULL
beta2n200=NULL
beta3n200=NULL
beta1n400=NULL
beta2n400=NULL
beta3n400=NULL
beta1n800=NULL
beta2n800=NULL
beta3n800=NULL
beta1n1000=NULL
beta2n1000=NULL
beta3n1000=NULL
sigma11n100=NULL
sigma12n100=NULL
sigma13n100=NULL
sigma22n100=NULL
sigma23n100=NULL
sigma33n100=NULL
sigma11n200=NULL
sigma12n200=NULL
sigma13n200=NULL
sigma22n200=NULL
sigma23n200=NULL
sigma33n200=NULL
sigma11n400=NULL
sigma12n400=NULL
sigma13n400=NULL
sigma22n400=NULL
sigma23n400=NULL
sigma33n400=NULL
sigma11n800=NULL
sigma12n800=NULL
sigma13n800=NULL
sigma22n800=NULL
sigma23n800=NULL
sigma33n800=NULL
sigma11n1000=NULL
sigma12n1000=NULL
sigma13n1000=NULL
sigma22n1000=NULL
sigma23n1000=NULL
sigma33n1000=NULL
nu1n100=NULL
nu2n100=NULL
nu3n100=NULL
nu1n200=NULL
nu2n200=NULL
nu3n200=NULL

nu1n400=NULL
nu2n400=NULL
nu3n400=NULL

nu1n800=NULL
nu2n800=NULL
nu3n800=NULL

nu1n1000=NULL
nu2n1000=NULL
nu3n1000=NULL

pii1n100=NULL
pii2n100=NULL
pii3n100=NULL
pii1n200=NULL
pii2n200=NULL
pii3n200=NULL
pii1n400=NULL
pii2n400=NULL
pii3n400=NULL
pii1n800=NULL
pii2n800=NULL
pii3n800=NULL
pii1n1000=NULL
pii2n1000=NULL
pii3n1000=NULL
for (i in 1:1000) {
  datosn100<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon1003misturas.RData")
  
  alpha1n100[i]=resultado[[i]]$alpha[1,]
  alpha2n100[i]=resultado[[i]]$alpha[2,]
  alpha3n100[i]=resultado[[i]]$alpha[3,]
  beta1n100[i]=resultado[[i]]$beta[1,]
  beta2n100[i]=resultado[[i]]$beta[2,]
  beta3n100[i]=resultado[[i]]$beta[3,]
  sigma11n100[i]=resultado[[i]]$Sigma[1,1]
  sigma12n100[i]=resultado[[i]]$Sigma[1,2]
  sigma13n100[i]=resultado[[i]]$Sigma[1,3]
  sigma22n100[i]=resultado[[i]]$Sigma[2,2]
  sigma23n100[i]=resultado[[i]]$Sigma[2,3]
  sigma33n100[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2],resultado[[i]]$nu[[3]][2]))
  nu1n100[i]=orden[1]
  nu2n100[i]=orden[2]
  nu3n100[i]=orden[3]
  
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2],resultado[[i]]$pii[3]))
  pii1n100[i]=orden[1]
  pii2n100[i]=orden[2]
  pii3n100[i]=orden[3]
  datosn200<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon2003misturas.RData")
  alpha1n200[i]=resultado[[i]]$alpha[1,]
  alpha2n200[i]=resultado[[i]]$alpha[2,]
  alpha3n200[i]=resultado[[i]]$alpha[3,]
  beta1n200[i]=resultado[[i]]$beta[1,]
  beta2n200[i]=resultado[[i]]$beta[2,]
  beta3n200[i]=resultado[[i]]$beta[3,]
  sigma11n200[i]=resultado[[i]]$Sigma[1,1]
  sigma12n200[i]=resultado[[i]]$Sigma[1,2]
  sigma13n200[i]=resultado[[i]]$Sigma[1,3]
  sigma22n200[i]=resultado[[i]]$Sigma[2,2]
  sigma23n200[i]=resultado[[i]]$Sigma[2,3]
  sigma33n200[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2],resultado[[i]]$nu[[3]][2]))
  nu1n200[i]=orden[1]
  nu2n200[i]=orden[2]
  nu3n200[i]=orden[3]
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2],resultado[[i]]$pii[3]))
  pii1n200[i]=orden[1]
  pii2n200[i]=orden[2]
  pii3n200[i]=orden[3]
  
  datosn400<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon4003misturas.RData")
  alpha1n400[i]=resultado[[i]]$alpha[1,]
  alpha2n400[i]=resultado[[i]]$alpha[2,]
  alpha3n400[i]=resultado[[i]]$alpha[3,]
  beta1n400[i]=resultado[[i]]$beta[1,]
  beta2n400[i]=resultado[[i]]$beta[2,]
  beta3n400[i]=resultado[[i]]$beta[3,]
  sigma11n400[i]=resultado[[i]]$Sigma[1,1]
  sigma12n400[i]=resultado[[i]]$Sigma[1,2]
  sigma13n400[i]=resultado[[i]]$Sigma[1,3]
  sigma22n400[i]=resultado[[i]]$Sigma[2,2]
  sigma23n400[i]=resultado[[i]]$Sigma[2,3]
  sigma33n400[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2],resultado[[i]]$nu[[3]][2]))
  nu1n400[i]=orden[1]
  nu2n400[i]=orden[2]
  nu3n400[i]=orden[3]
  
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2],resultado[[i]]$pii[3]))
  pii1n400[i]=orden[1]
  pii2n400[i]=orden[2]
  pii3n400[i]=orden[3]
  datosn800<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon8003misturas.RData")
  alpha1n800[i]=resultado[[i]]$alpha[1,]
  alpha2n800[i]=resultado[[i]]$alpha[2,]
  alpha3n800[i]=resultado[[i]]$alpha[3,]
  beta1n800[i]=resultado[[i]]$beta[1,]
  beta2n800[i]=resultado[[i]]$beta[2,]
  beta3n800[i]=resultado[[i]]$beta[3,]
  sigma11n800[i]=resultado[[i]]$Sigma[1,1]
  sigma12n800[i]=resultado[[i]]$Sigma[1,2]
  sigma13n800[i]=resultado[[i]]$Sigma[1,3]
  sigma22n800[i]=resultado[[i]]$Sigma[2,2]
  sigma23n800[i]=resultado[[i]]$Sigma[2,3]
  sigma33n800[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2],resultado[[i]]$nu[[3]][2]))
  nu1n800[i]=orden[1]
  nu2n800[i]=orden[2]
  nu3n800[i]=orden[3]
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2],resultado[[i]]$pii[3]))
  pii1n800[i]=orden[1]
  pii2n800[i]=orden[2]
  pii3n800[i]=orden[3]
  datosn1000<-load(file = "/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/resultados simulaciones/resultadon10003misturas.RData")
  alpha1n1000[i]=resultado[[i]]$alpha[1,]
  alpha2n1000[i]=resultado[[i]]$alpha[2,]
  alpha3n1000[i]=resultado[[i]]$alpha[3,]
  beta1n1000[i]=resultado[[i]]$beta[1,]
  beta2n1000[i]=resultado[[i]]$beta[2,]
  beta3n1000[i]=resultado[[i]]$beta[3,]
  sigma11n1000[i]=resultado[[i]]$Sigma[1,1]
  sigma12n1000[i]=resultado[[i]]$Sigma[1,2]
  sigma13n1000[i]=resultado[[i]]$Sigma[1,3]
  sigma22n1000[i]=resultado[[i]]$Sigma[2,2]
  sigma23n1000[i]=resultado[[i]]$Sigma[2,3]
  sigma33n1000[i]=resultado[[i]]$Sigma[3,3]
  orden=sort(c(resultado[[i]]$nu[[1]][2],resultado[[i]]$nu[[2]][2],resultado[[i]]$nu[[3]][2]))
  nu1n1000[i]=orden[1]
  nu2n1000[i]=orden[2]
  nu3n1000[i]=orden[3]
  orden=sort(c(resultado[[i]]$pii[1],resultado[[i]]$pii[2],resultado[[i]]$pii[3]))
  pii1n1000[i]=orden[1]
  pii2n1000[i]=orden[2]
  pii3n1000[i]=orden[3]
}
par(mfrow=c(1,1))
alpha1<-data.frame(n100=alpha1n100,n200=alpha1n200,n400=alpha1n400,n800=alpha1n800,n1000=alpha1n1000)
boxplot(alpha1,col =grey.colors(5, start = 0.2, end = 1),pch=16,ylim=c(-0.6,0.6)
        ,main=expression(paste( alpha[1] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=0,b=0)
#legend("bottom", inset=.02, c("100","200","400","800","100"), fill=grey.colors(4, start = 0.2, end = 1), cex=1.2)
legend("bottom", inset=0.01, legend=c("100", "200", "400", "800", "1000"),
       fill=grey.colors(5, start=0.2, end=1), horiz=TRUE, cex=1, x.intersp=0.5)

alpha2<-data.frame(n100=alpha2n100,n200=alpha2n200,n400=alpha2n400,n800=alpha2n800,n1000=alpha2n1000)
boxplot(alpha2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( alpha[2] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=-0.2,b=0)
alpha3<-data.frame(n100=alpha3n100,n200=alpha3n200,n400=alpha3n400,n800=alpha3n800,n1000=alpha3n1000)
boxplot(alpha3,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( alpha[3] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=0.3,b=0)
#######beta
beta1<-data.frame(n100=beta1n100,n200=beta1n200,n400=beta1n400,n800=beta1n800,n1000=beta1n1000)
boxplot(beta1,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( beta[1] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
#legend("bottom", inset=.02, c("100","200","400","800","100"), fill=grey.colors(4, start = 0.2, end = 1), cex=1.2)
#legend("bottom", inset=-0.01, legend=c("100", "200", "400", "800", "1000"),
#       fill=grey.colors(5, start=0.2, end=0.8), horiz=TRUE, cex=1, x.intersp=0.5)

beta2<-data.frame(n100=beta2n100,n200=beta2n200,n400=beta2n400,n800=beta2n800,n1000=beta2n1000)
boxplot(beta2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( beta[2] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1.5,b=0)
beta3<-data.frame(n100=beta3n100,n200=beta3n200,n400=beta3n400,n800=beta3n800,n1000=beta3n1000)
boxplot(beta3,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( beta[3] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=0.6,b=0)
#####sigma
sigma11<-data.frame(n100=sigma11n100,n200=sigma11n200,n400=sigma11n400,n800=sigma11n800,n1000=sigma11n1000)
boxplot(beta1,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[11] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
#legend("bottom", inset=.02, c("100","200","400","800","100"), fill=grey.colors(4, start = 0.2, end = 1), cex=1.2)
#legend("bottom", inset=-0.01, legend=c("100", "200", "400", "800", "1000"),
#       fill=grey.colors(5, start=0.2, end=0.8), horiz=TRUE, cex=1, x.intersp=0.5)
sigma12<-data.frame(n100=sigma12n100,n200=sigma12n200,n400=sigma12n400,n800=sigma12n800,n1000=sigma12n1000)
boxplot(sigma12,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[12] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
sigma13<-data.frame(n100=sigma13n100,n200=sigma13n200,n400=sigma13n400,n800=sigma13n800,n1000=sigma13n1000)
boxplot(sigma13,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[13] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
sigma22<-data.frame(n100=sigma22n100,n200=sigma22n200,n400=sigma22n400,n800=sigma22n800,n1000=sigma22n1000)
boxplot(sigma22,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[22] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=2.5,b=0)
sigma23<-data.frame(n100=sigma23n100,n200=sigma23n200,n400=sigma23n400,n800=sigma23n800,n1000=sigma23n1000)
boxplot(sigma23,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[23] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
sigma33<-data.frame(n100=sigma33n100,n200=sigma33n200,n400=sigma33n400,n800=sigma33n800,n1000=sigma33n1000)
boxplot(sigma33,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( sigma[33] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=5,b=0)
#####################nu
nu1<-data.frame(n100=nu1n100,n200=nu1n200,n400=nu1n400,n800=nu1n800,n1000=nu1n1000)
boxplot(nu1,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( psi[1] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=1,b=0)
#legend("bottom", inset=.02, c("100","200","400","800","100"), fill=grey.colors(4, start = 0.2, end = 1), cex=1.2)
#legend("bottom", inset=-0.01, legend=c("100", "200", "400", "800", "1000"),
#       fill=grey.colors(5, start=0.2, end=0.8), horiz=TRUE, cex=1, x.intersp=0.5)

nu2<-data.frame(n100=nu2n100,n200=nu2n200,n400=nu2n400,n800=nu2n800,n1000=nu2n1000)
boxplot(nu2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( psi[2] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=2,b=0)
nu3<-data.frame(n100=nu3n100,n200=nu3n200,n400=nu3n400,n800=nu3n800,n1000=nu3n1000)
boxplot(nu2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( psi[3] )),names = F,cex.main=1.5,cex.axis=1.5)
abline(a=3,b=0)

#######pii
pii1<-data.frame(n100=pii1n100,n200=pii1n200,n400=pii1n400,n800=pii1n800,n1000=pii1n1000)
boxplot(pii1,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( p[1] )),names = F,cex.main=1.5,cex.axis=1.5)#,ylim=c(-0.4,1))
abline(a=0.2,b=0)
pii2<-data.frame(n100=pii2n100,n200=pii2n200,n400=pii2n400,n800=pii2n800,n1000=pii2n1000)
boxplot(pii2,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( p[2] )),names = F,cex.main=1.5,cex.axis=1.5)#,ylim=c(0,1.2))
abline(a=0.3,b=0)
pii3<-data.frame(n100=pii3n100,n200=pii3n200,n400=pii3n400,n800=pii3n800,n1000=pii3n1000)

boxplot(pii3,col =grey.colors(5, start = 0.2, end = 1),pch=16
        ,main=expression(paste( p[3] )),names = F,cex.main=1.5,cex.axis=1.5)#,ylim=c(0,1.2))
abline(a=0.5,b=0)

fila0<-(cbind(mean(nu1$n200),median(nu1$n200),sd(nu1$n200),mean(nu1$n400),median(nu1$n400),sd(nu1$n400)))
fila0.1<-(cbind(mean(nu2$n200),median(nu2$n200),sd(nu2$n200),mean(nu2$n400),median(nu2$n400),sd(nu2$n400)))
fila0.11<-(cbind(mean(nu3$n200),median(nu3$n200),sd(nu3$n200),mean(nu3$n400),median(nu3$n400),sd(nu3$n400)))
fila1<-(cbind(mean(alpha1$n200),median(alpha1$n200),sd(alpha1$n200),mean(alpha1$n400),median(alpha1$n400),sd(alpha1$n400)))
fila2<-(cbind(mean(alpha2$n200),median(alpha2$n200),sd(alpha2$n200),mean(alpha2$n400),median(alpha2$n400),sd(alpha2$n400)))
fila3<-(cbind(mean(alpha3$n200),median(alpha3$n200),sd(alpha3$n200),mean(alpha3$n400),median(alpha3$n400),sd(alpha3$n400)))
fila4<-(cbind(mean(beta1$n200),median(beta1$n200),sd(beta1$n200),mean(beta1$n400),median(beta1$n400),sd(beta1$n400)))
fila5<-(cbind(mean(beta2$n200),median(beta2$n200),sd(beta2$n200),mean(beta2$n400),median(beta2$n400),sd(beta2$n400)))
fila6<-(cbind(mean(beta3$n200),median(beta3$n200),sd(beta3$n200),mean(beta3$n400),median(beta3$n400),sd(beta3$n400)))
fila7<-(cbind(mean(sigma11$n200),median(sigma11$n200),sd(sigma11$n200),mean(sigma11$n400),median(sigma11$n400),sd(sigma11$n400)))
fila8<-(cbind(mean(sigma12$n200),median(sigma12$n200),sd(sigma12$n200),mean(sigma12$n400),median(sigma12$n400),sd(sigma12$n400)))
fila9<-(cbind(mean(sigma13$n200),median(sigma13$n200),sd(sigma13$n200),mean(sigma13$n400),median(sigma13$n400),sd(sigma13$n400)))
fila10<-(cbind(mean(sigma22$n200),median(sigma22$n200),sd(sigma22$n200),mean(sigma22$n400),median(sigma22$n400),sd(sigma22$n400)))
fila11<-(cbind(mean(sigma23$n200),median(sigma23$n200),sd(sigma23$n200),mean(sigma23$n400),median(sigma23$n400),sd(sigma23$n400)))
fila12<-(cbind(mean(sigma33$n200),median(sigma33$n200),sd(sigma33$n200),mean(sigma33$n400),median(sigma33$n400),sd(sigma33$n400)))
fila13<-(cbind(mean(pii1$n200),median(pii1$n200),sd(pii1$n200),mean(pii1$n400),median(pii1$n400),sd(pii1$n400)))
fila14<-(cbind(mean(pii2$n200),median(pii2$n200),sd(pii2$n200),mean(pii2$n400),median(pii2$n400),sd(pii2$n400)))
fila15<-(cbind(mean(pii3$n200),median(pii3$n200),sd(pii3$n200),mean(pii3$n400),median(pii3$n400),sd(pii3$n400)))
parte1 <- rbind(fila0,fila0.1,fila0.11,fila1, fila2, fila3, fila4, fila5, fila6, fila7, fila8, fila9, fila10, fila11, fila12, fila13, fila14,fila15)
fila0<-(cbind(mean(nu1$n800),median(nu1$n800),sd(nu1$n800),mean(nu1$n1000),median(nu1$n1000),sd(nu1$n1000)))
fila0.1<-(cbind(mean(nu2$n800),median(nu2$n800),sd(nu2$n800),mean(nu2$n1000),median(nu2$n1000),sd(nu2$n1000)))
fila0.11<-(cbind(mean(nu3$n800),median(nu3$n800),sd(nu3$n800),mean(nu3$n1000),median(nu3$n1000),sd(nu3$n1000)))
fila1<-(cbind(mean(alpha1$n800),median(alpha1$n800),sd(alpha1$n800),mean(alpha1$n1000),median(alpha1$n1000),sd(alpha1$n1000)))
fila2<-(cbind(mean(alpha2$n800),median(alpha2$n800),sd(alpha2$n800),mean(alpha2$n1000),median(alpha2$n1000),sd(alpha2$n1000)))
fila3<-(cbind(mean(alpha3$n800),median(alpha3$n800),sd(alpha3$n800),mean(alpha3$n1000),median(alpha3$n1000),sd(alpha3$n1000)))
fila4<-(cbind(mean(beta1$n800),median(beta1$n800),sd(beta1$n800),mean(beta1$n1000),median(beta1$n1000),sd(beta1$n1000)))
fila5<-(cbind(mean(beta2$n800),median(beta2$n800),sd(beta2$n800),mean(beta2$n1000),median(beta2$n1000),sd(beta2$n1000)))
fila6<-(cbind(mean(beta3$n800),median(beta3$n800),sd(beta3$n800),mean(beta3$n1000),median(beta3$n1000),sd(beta3$n1000)))
fila7<-(cbind(mean(sigma11$n800),median(sigma11$n800),sd(sigma11$n800),mean(sigma11$n1000),median(sigma11$n1000),sd(sigma11$n1000)))
fila8<-(cbind(mean(sigma12$n800),median(sigma12$n800),sd(sigma12$n800),mean(sigma12$n1000),median(sigma12$n1000),sd(sigma12$n1000)))
fila9<-(cbind(mean(sigma13$n800),median(sigma13$n800),sd(sigma13$n800),mean(sigma13$n1000),median(sigma13$n1000),sd(sigma13$n1000)))
fila10<-(cbind(mean(sigma22$n800),median(sigma22$n800),sd(sigma22$n800),mean(sigma22$n1000),median(sigma22$n1000),sd(sigma22$n1000)))
fila11<-(cbind(mean(sigma23$n800),median(sigma23$n800),sd(sigma23$n800),mean(sigma23$n1000),median(sigma23$n1000),sd(sigma23$n1000)))
fila12<-(cbind(mean(sigma33$n800),median(sigma33$n800),sd(sigma33$n800),mean(sigma33$n1000),median(sigma33$n1000),sd(sigma33$n1000)))
fila13<-(cbind(mean(pii1$n800),median(pii1$n800),sd(pii1$n800),mean(pii1$n1000),median(pii1$n1000),sd(pii1$n1000)))
fila14<-(cbind(mean(pii2$n800),median(pii2$n800),sd(pii2$n800),mean(pii2$n1000),median(pii2$n1000),sd(pii2$n1000)))
fila15<-(cbind(mean(pii3$n800),median(pii3$n800),sd(pii3$n800),mean(pii3$n1000),median(pii3$n1000),sd(pii3$n1000)))
parte2 <- rbind(fila0,fila0.1,fila0.11,fila1, fila2, fila3, fila4, fila5, fila6, fila7, fila8, fila9, fila10, fila11, fila12, fila13, fila14,fila15)

tabla<-rbind(parte1,parte2)
xtable::xtable(tabla,digits=4)
