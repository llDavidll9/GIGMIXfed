rm(list=ls(all=TRUE))


#install.packages('ghyp')
#install.packages('expm')
#install.packages('mvtnorm')
#install.packages('mixsmsn')
require(ghyp)
require(expm)
require(mvtnorm)
require(mixsmsn)
#/Users/davidesteban/Documents/David/Tesis Mestrado
#/Users/davidesteban/Downloads
#### mudar o diretório de trabalho ###########
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/gera2.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/densidades.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/capm.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/capmini.txt")


############################################# Simulando ################################
set.seed(123)
n=500
p=2

alpha=matrix(c(0.5,0.5),p,1)
beta=matrix(c(1,2),p,1)
x=rnorm(n,1,2)

mu=matrix(0,n,p)
for (hh in 1:n){      
  mu[hh,]<-alpha+beta*x[hh]
}

Sigma= matrix(c(1,0.5,0.5,2), 2,2)

mu1<-c(0,0) 
Sigma1 <-Sigma
shape1 <-c(0,0)
eta1<-0.5  
omega1<-2
nu1 <-c(eta1,omega1,omega1)  # considerando a reparametrização psi=gamma

mu2<-c(0,0)
Sigma2 <- Sigma1
eta2<--0.5
omega2=2
nu2 <-c(eta2,omega2,omega2)  # considerando a reparametrização psi=gamma
shape2 <-c(0,0)
pii<-c(0.3,0.7)


arg1 = list(mu=mu1, Sigma=Sigma1, shape=shape1, nu=nu1)
arg2 = list(mu=mu2, Sigma=Sigma2, shape=shape2, nu=nu2)
y1 <- rmmix(n=n, pii = pii, "GIG", list(arg1,arg2))#"GIGCAPM"
y<-y1+mu
nu<-list(nu1,nu2)
modeln<-smsn.mmix(y1, nu=1, mu =NULL, Sigma = NULL, shape = NULL, pii = NULL, g = 2, get.init = TRUE, criteria = FALSE,
                  group = FALSE, family = "Normal", error = 0.0001, iter.max = 100, uni.Gama = T, calc.im=FALSE,
                  obs.prob= FALSE, kmeans.param = NULL)
resini=aprox.inicial(y,condi=T,g1=2)
resini$pii<-rev(modeln$pii)
saida=EM(y, x, alpha, beta, Sigma, nu, pii,g=2,iter.max=100,error=0.00001,condi=T, equal=T)
saida=EM(y, x, resini$alpha, resini$beta, resini$Sigma, resini$nu, resini2$pii,g=2,iter.max=100,error=0.00001,condi=T, equal=T)
saida



####simulaciones
resultado = list()
n = 500
p = 2

for (hh in 1:1000) {
  
  alpha = matrix(c(0.5, 0.5), p, 1)
  beta = matrix(c(1, 2), p, 1)
  x = rnorm(n, 1, 2)
  
  mu = matrix(0, n, p)
  for (i in 1:n) {
    mu[i, ] = alpha + beta * x[i]
  }
  
  Sigma = matrix(c(1, 0.5, 0.5, 2), 2, 2)
  
  mu1 = c(0, 0)
  Sigma1 = Sigma
  shape1 = c(0, 0)
  eta1 = 0.5
  omega1 = 2
  nu1 = c(eta1, omega1, omega1)
  
  mu2 = c(0, 0)
  Sigma2 = Sigma
  eta2 = -0.5
  omega2 = 2
  nu2 = c(eta2, omega2, omega2)
  shape2 = c(0, 0)
  pii = c(0.3, 0.7)
  
  arg1 = list(mu = mu1, Sigma = Sigma1, shape = shape1, nu = nu1)
  arg2 = list(mu = mu2, Sigma = Sigma2, shape = shape2, nu = nu2)
  y1 = rmmix(n = n, pii = pii, "GIG", list(arg1, arg2))
  y = y1 + mu
  nu = list(nu1, nu2)
  modeln<-smsn.mmix(y1, nu=1, mu =NULL, Sigma = NULL, shape = NULL, pii = NULL, g = 2, get.init = TRUE, criteria = FALSE,
                    group = FALSE, family = "Normal", error = 0.0001, iter.max = 100, uni.Gama = T, calc.im=FALSE,
                    obs.prob= FALSE, kmeans.param = NULL)
  resini=aprox.inicial(y,condi=T,g1=2)
  resini$pii<-rev(modeln$pii)
  
  #EM(y, x, alpha, beta, Sigma, nu, pii,g=2,iter.max=100,error=0.00001,condi=T, equal=T)
  
  
  resultado[[hh]] =EM(y, x, resini$alpha, resini$beta, resini$Sigma, resini$nu
                      , resini2$pii,g=2,iter.max=100,error=0.00001,condi=T, equal=T)
  print(hh)
}

mont=1000
alphamont=matrix(0,p,mont)
betamont=matrix(0,p,mont)
Sigmamont=c()
Sigmamont2=c()
Sigmamont3=c()
numont1=matrix(0,p,mont)
numont2=matrix(0,p,mont)
sigmaTT=matrix(0,p,p)
res=resultado
piimont=matrix(0,p,mont)
for (jj in 1:mont){
  alphamont[,jj]=res[[jj]]$alpha
  betamont[,jj]=res[[jj]]$beta
  Sigma=res[[jj]]$Sigma
  Sigmaante=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][1]
  Sigmaante2=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][2]
  Sigmaante3=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][3]
  Sigmamont[jj]=Sigmaante
  Sigmamont2[jj]=Sigmaante2
  Sigmamont3[jj]=Sigmaante3
  numont1[,jj]=res[[jj]]$nu[[1]]
  numont2[,jj]=res[[jj]]$nu[[2]]
  sigmaTT=res[[jj]]$Sigma+sigmaTT
  piimont[,jj]=res[[jj]]$pii
}
#save(res,file ="resn500nu9.RData")
#save(erropad,file="erropadn500nu9.RData")
#save(tt,file="tt500nu9.RData")

apply(alphamont,1,mean)
apply(alphamont,1,sd)

apply(betamont,1,mean)
apply(betamont,1,sd)

mean(Sigmamont)
sd(Sigmamont)
mean(Sigmamont2)
sd(Sigmamont2)
mean(Sigmamont3)
sd(Sigmamont3)
apply(piimont,1,mean)






######svm
#############3dimensiones
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/EMini.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/EMsmngig.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/capmini.txt")
resultado = list()
n = 600
p = 3
g=2
library(e1071)
mont=100

for (hh in 1:mont) {
  
  alpha = matrix(c(0,-0.2,0.3), p, 1)
  beta = matrix(c(1, 1.5,0.6), p, 1)
  x = rnorm(n, 1, 2)
  
  mu = matrix(0, n, p)
  for (i in 1:n) {
    mu[i, ] = alpha + beta * x[i]
  }
  
  Sigma = matrix(c(1, 1, 1, 1,2.5,1,1,1,5), 3, 3)
  
  mu1 = c(0, 0,0)
  Sigma1 = Sigma
  shape1 = c(0, 0,0)
  eta1 = 0.5
  omega1 = 1
  nu1 = c(eta1, omega1, omega1)
  mu2 = c(0, 0,0)
  Sigma2 = Sigma
  eta2 = -0.5
  omega2 = 2
  nu2 = c(eta2, omega2, omega2)
  shape2 = c(0, 0,0)
  pii = c(0.3, 0.7)
  
  arg1 = list(mu = mu1, Sigma = Sigma1, shape = shape1, nu = nu1)
  arg2 = list(mu = mu2, Sigma = Sigma2, shape = shape2, nu = nu2)
  y1 = rmmix(n = n, pii = pii, "GIG", list(arg1, arg2))
  y = y1 + mu
  nu = list(nu1, nu2)
  #svm_model <- svm(y, kernel = "polynomial",degree=1,gamma = 0.7)
  modelt<-smsn.mmix(y, nu=100, mu =NULL, Sigma = NULL, shape = NULL, pii = NULL, g = g, get.init = TRUE, criteria = FALSE,
                    group = FALSE, family =  "t", error = 0.0001, iter.max = 100, uni.Gama = T, calc.im=FALSE,
                    obs.prob= FALSE, kmeans.param = NULL)
  nu2.2=c(eta2,modelt$nu,modelt$nu)
  nu1.1=c(eta1,modelt$nu,modelt$nu)
  nuini=list(nu1.1,nu2.2)
  # Display the model summary
  
  #predictions <- predict(svm_model, y)
  resini=aprox.inicial(y,x,condi=T,equal=F,g1=2,nuini)
  #resini2=aprox.inicial2(y,condi=F,g1=2)
  #resini$nu[[2]]=resini2$nu[[2]]
  #resini$nu[[1]][1] <- eta1
  #resini$nu[[2]][1] <- eta2
  
  #resini$pii<-sort(rev(table(predictions)/n))
  #resini$Sigma<-cov(y1)
  #best_parameters<-grid_search_log_likelihood_nu2(y, x, resini$alpha, resini$beta, 
  #                                                resini$Sigma, resini$nu, resini$pii, 
  #                                                grid_size = 0.2)
  #resultado[[hh]] =EM(y, x, alpha, beta, Sigma, nu, pii,g=2,iter.max=100,error=0.00001,condi=T, equal=F)
  best_parameters <- grid_search_log_likelihood(y, x, resini$alpha, resini$beta, 
                                                resini$Sigma, resini$nu, resini$pii, 
                                                grid_size = 0.3) 
  resini$nu[[1]][2]<-best_parameters$best_nu1
  resini$nu[[2]][2]<-best_parameters$best_nu2
  
  resultado[[hh]] =EM(y, x, resini$alpha, resini$beta, resini$Sigma, resini$nu
                      , resini$pii,g=2,iter.max=100,error=0.00001,condi=T, equal=F)
  print(hh)
}
save(resultado,file="resultadon100.2misturas.RData")

alphamont=matrix(0,p,mont)
betamont=matrix(0,p,mont)
Sigmamont=c()
Sigmamont2=c()
Sigmamont3=c()
Sigmamont4=c()
Sigmamont5=c()
Sigmamont6=c()
numont1=matrix(0,2,mont)
numont2=matrix(0,2,mont)
sigmaTT=matrix(0,p,p)
res=resultado
piimont=matrix(0,2,mont)
for (jj in 1:mont){
  alphamont[,jj]=res[[jj]]$alpha
  betamont[,jj]=res[[jj]]$beta
  Sigma=res[[jj]]$Sigma
  Sigmaante=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][1]
  Sigmaante2=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][2]
  Sigmaante3=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][3]
  Sigmaante4=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][4]
  Sigmaante5=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][5]
  Sigmaante6=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][6]
  Sigmamont[jj]=Sigmaante
  Sigmamont2[jj]=Sigmaante2
  Sigmamont3[jj]=Sigmaante3
  Sigmamont4[jj]=Sigmaante4
  Sigmamont5[jj]=Sigmaante5
  Sigmamont6[jj]=Sigmaante6
  numont1[,jj]=res[[jj]]$nu[[1]]
  numont2[,jj]=res[[jj]]$nu[[2]]
  sigmaTT=res[[jj]]$Sigma+sigmaTT
  piimont[,jj]=res[[jj]]$pii
}
#save(res,file ="resn500nu9.RData")
#save(erropad,file="erropadn500nu9.RData")
#save(tt,file="tt500nu9.RData")
library(xtable)
xtable(cbind(
  c(
    apply(numont1, 1, mean),
    apply(numont2, 1, mean),
    apply(alphamont, 1, mean),
    apply(betamont, 1, mean),
    mean(Sigmamont),
    mean(Sigmamont2),
    mean(Sigmamont3),
    mean(Sigmamont4),
    mean(Sigmamont5),
    mean(Sigmamont6),
    apply(piimont, 1, mean)
  ),
  c(
    apply(numont1, 1, median),
    apply(numont2, 1, median),
    apply(alphamont, 1, median),
    apply(betamont, 1, median),
    median(Sigmamont),
    median(Sigmamont2),
    median(Sigmamont3),
    median(Sigmamont4),
    median(Sigmamont5),
    median(Sigmamont6),
    apply(piimont, 1, median)
  ),
  c(
    apply(numont1, 1, sd),
    apply(numont2, 1, sd),
    apply(alphamont, 1, sd),
    apply(betamont, 1, sd),
    sd(Sigmamont),
    sd(Sigmamont2),
    sd(Sigmamont3),
    sd(Sigmamont4),
    sd(Sigmamont5),
    sd(Sigmamont6),
    apply(piimont, 1, sd)
  )
),digits=4)

######svm
#############3dimensiones
#############3 misturas
source("/Users/davidesteban/Downloads/gerageral2.txt")
source("/Users/davidesteban/Downloads/capmini.txt")

resultado = list()
n = 600
p = 3
library(e1071)
g=3
mont=100
for (hh in 1:mont) {
  
  alpha = matrix(c(0,-0.2,0.3), p, 1)
  beta = matrix(c(1, 1.5,0.6), p, 1)
  x = rnorm(n, 1, 2)
  
  mu = matrix(0, n, p)
  for (i in 1:n) {
    mu[i, ] = alpha + beta * x[i]
  }
  
  Sigma = matrix(c(1, 1, 1, 1,2.5,1,1,1,5), 3, 3)
  
  mu1 = c(0, 0,0)
  Sigma1 = Sigma
  shape1 = c(0, 0,0)
  eta1 = 0.5
  omega1 = 1
  nu1 = c(eta1, omega1, omega1)

  mu2 = c(0, 0,0)
  Sigma2 = Sigma
  eta2 = -0.5
  omega2 = 2
  nu2 = c(eta2, omega2, omega2)
  shape2 = c(0, 0,0)

  
  mu3 = c(0, 0,0)
  Sigma3 = Sigma
  eta3 = 1
  omega3 = 3
  nu3 = c(eta3, omega3, omega3)
  shape3 = c(0, 0,0)

  pii = c(0.2,0.3, 0.5)
  
  arg1 = list(mu = mu1, Sigma = Sigma1, shape = shape1, nu = nu1)
  arg2 = list(mu = mu2, Sigma = Sigma2, shape = shape2, nu = nu2)
  arg3 = list(mu = mu3, Sigma = Sigma3, shape = shape3, nu = nu3)
  
  y1 = rmmix(n = n, pii = pii, "GIG", list(arg1, arg2, arg3))
  y = y1 + mu
  nu = list(nu1, nu2, nu3)
  nuini=list(nu1.1,nu2.2,nu3.3)
  #svm_model <- svm(y, kernel = "polynomial",degree=2, type = "C-classification")
  
  
  #predictions <- predict(svm_model, y1)
  modeln<-smsn.mmix(y, nu=1, mu =NULL, Sigma = NULL, shape = NULL, pii = NULL, g = g, get.init = TRUE, criteria = FALSE,
                    group = FALSE, family = "Normal", error = 0.0001, iter.max = 100, uni.Gama = T, calc.im=FALSE,
                    obs.prob= FALSE, kmeans.param = NULL)
  modelt<-smsn.mmix(y, nu=100, mu =NULL, Sigma = NULL, shape = NULL, pii = NULL, g = g, get.init = TRUE, criteria = FALSE,
                    group = FALSE, family =  "t", error = 0.0001, iter.max = 100, uni.Gama = T, calc.im=FALSE,
                    obs.prob= FALSE, kmeans.param = NULL)
  nu2.2=c(eta2,sqrt(modelt$nu),sqrt(modelt$nu))
  nu1.1=c(eta1,sqrt(modelt$nu),sqrt(modelt$nu))
  nu3.3=c(eta3,sqrt(modelt$nu),sqrt(modelt$nu))
  nuini=list(nu1.1,nu2.2,nu3.3)
  # Step 1: Cluster the data using K-means to generate 3 clusters
  kmeans_result <- kmeans(y, centers = 3)
  
  # Get the cluster labels generated by K-means
  cluster_labels <- kmeans_result$cluster
  # Step 2: Train the SVM model using the clustered labels
  svm_model <- svm(y, as.factor(cluster_labels), type = "C-classification"
                   , kernel = "polynomial",degree = 3)
  
  # Step 3: Use the trained SVM model to classify new data
  # Example of predicting for the same data (you can replace with new data)
  predicted_clusters <- predict(svm_model, y)
  #sort(table(predicted_clusters)/n)
  resini=aprox.inicial(y,x,condi=T,equal=F,3,nuini)
  #resini2=aprox.inicial3(y,x,condi=T,equal=F,3,nuini)
  #resini$pii<-sort(modeln$pii)  #rev(table(predictions)/500)
  resini$pii<-sort(table(predicted_clusters)/n)
  #resini$Sigma<-cov(y1)
  
  #best_parameters<-grid_search_log_likelihood3nu2nu3fixed(y,x, resini$alpha, resini$beta, 
  #                                                     resini$Sigma, resini$nu, resini$pii, 
  #                                                     grid_size = 0.8)
  #resini$nu[[3]][2]<-best_parameters$best_nu3
  
  
  orden<-sort(c(resini$nu[[1]][2],resini$nu[[2]][2],resini$nu[[3]][2]))
  resini$nu[[1]][2]<-orden[1]
  resini$nu[[2]][2]<-orden[2]
  resini$nu[[3]][2]<-orden[3]
  
  
  #best_parameters <- grid_search_log_likelihood3nu1fixed(y, x, resini$alpha, resini$beta, 
  #                                                       resini$Sigma, resini$nu, resini$pii, 
  #                                                       grid_size = 0.1) 
  #resini$nu[[1]][2]<-best_parameters$best_nu1
  #best_parameters <- grid_search_log_likelihood3nu2fixed(y, x, resini$alpha, resini$beta, 
  #                                                       resini$Sigma, resini$nu, resini$pii, 
  #                                                       grid_size = 0.4) 
  #resini$nu[[2]][2]<-best_parameters$best_nu2
  
  
  best_parameters <- grid_search_log_likelihood3nu1nu3fixed(y, x, resini$alpha, resini$beta, 
                                                 resini$Sigma, resini$nu, resini$pii, 
                                                 grid_size = 0.3) 
  resini$nu[[1]][2]<-best_parameters$best_nu1
  #resini$nu[[3]][2]<-best_parameters$best_nu3
  #best_parameters<-grid_search_log_likelihood3nu3fixed(y,x, resini$alpha, resini$beta, 
  #                                                     resini$Sigma, resini$nu, resini$pii, 
  #                                                     grid_size = 1.3)
  #best_parameters <- grid_search_log_likelihood3(y, x, resini$alpha, resini$beta, 
  #                                              resini$Sigma, resini$nu, resini$pii, 
  #                                              grid_size = 0.2) 
  #resini$nu[[2]][2]<-best_parameters$best_nu2
  #resini$nu[[3]][2]<-best_parameters$best_nu3
  resultado[[hh]] =EM2(y, x, resini$alpha, resini$beta, resini$Sigma, resini$nu
                      , resini$pii,g=3,iter.max=100,error=0.00001,condi=T, equal=F)
  #resultado[[hh]]
  #resultado[[hh]] =EM2(y, x, alpha, beta, Sigma, nu, pii,g=3,iter.max=100,error=0.00001,condi=T, equal=F)
  
  print(hh)
}
save(resultado,file="resultadon6003misturas.RData")

alphamont=matrix(0,p,mont)
betamont=matrix(0,p,mont)
Sigmamont=c()
Sigmamont2=c()
Sigmamont3=c()
Sigmamont4=c()
Sigmamont5=c()
Sigmamont6=c()
numont1=matrix(0,2,mont)
numont2=matrix(0,2,mont)
numont3=matrix(0,2,mont)
sigmaTT=matrix(0,p,p)
res=resultado
piimont=matrix(0,3,mont)
for (jj in 1:mont){
  alphamont[,jj]=res[[jj]]$alpha
  betamont[,jj]=res[[jj]]$beta
  Sigma=res[[jj]]$Sigma
  Sigmaante=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][1]
  Sigmaante2=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][2]
  Sigmaante3=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][3]
  Sigmaante4=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][4]
  Sigmaante5=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][5]
  Sigmaante6=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][6]
  Sigmamont[jj]=Sigmaante
  Sigmamont2[jj]=Sigmaante2
  Sigmamont3[jj]=Sigmaante3
  Sigmamont4[jj]=Sigmaante4
  Sigmamont5[jj]=Sigmaante5
  Sigmamont6[jj]=Sigmaante6
  orden=sort(c(res[[jj]]$nu[[1]][2],res[[jj]]$nu[[2]][2],res[[jj]]$nu[[3]][2]))
  res[[jj]]$nu[[1]][2]<-orden[1]
  res[[jj]]$nu[[2]][2]<-orden[2]
  res[[jj]]$nu[[3]][2]<-orden[3]
  numont1[,jj]=res[[jj]]$nu[[1]]
  numont2[,jj]=res[[jj]]$nu[[2]]
  numont3[,jj]=res[[jj]]$nu[[3]]
  sigmaTT=res[[jj]]$Sigma+sigmaTT
  piimont[,jj]=res[[jj]]$pii
}
#save(res,file ="resn500nu9.RData")
#save(erropad,file="erropadn500nu9.RData")
#save(tt,file="tt500nu9.RData")
library(xtable)
xtable(cbind(
  c(
    apply(numont1, 1, mean),
    apply(numont2, 1, mean),
    apply(numont3, 1, mean),
    apply(alphamont, 1, mean),
    apply(betamont, 1, mean),
    mean(Sigmamont),
    mean(Sigmamont2),
    mean(Sigmamont3),
    mean(Sigmamont4),
    mean(Sigmamont5),
    mean(Sigmamont6),
    apply(piimont, 1, mean)
  ),
  c(
    apply(numont1, 1, median),
    apply(numont2, 1, median),
    apply(numont3, 1, median),
    apply(alphamont, 1, median),
    apply(betamont, 1, median),
    median(Sigmamont),
    median(Sigmamont2),
    median(Sigmamont3),
    median(Sigmamont4),
    median(Sigmamont5),
    median(Sigmamont6),
    apply(piimont, 1, median)
  ),
  c(
    apply(numont1, 1, sd),
    apply(numont2, 1, sd),
    apply(numont3, 1, sd),
    apply(alphamont, 1, sd),
    apply(betamont, 1, sd),
    sd(Sigmamont),
    sd(Sigmamont2),
    sd(Sigmamont3),
    sd(Sigmamont4),
    sd(Sigmamont5),
    sd(Sigmamont6),
    apply(piimont, 1, sd)
  )
),digits=4)


################svm Camila proposta
######svm
#############3dimensiones
#############3 misturas
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/gerageral2.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/capmini.txt")

resultado = list()
n = 600
p = 3
library(e1071)
g=3
mont=100
for (hh in 1:mont) {
  
  alpha = matrix(c(0,-0.2,0.3), p, 1)
  beta = matrix(c(1, 1.5,0.6), p, 1)
  x = rnorm(n, 1, 2)
  
  mu = matrix(0, n, p)
  for (i in 1:n) {
    mu[i, ] = alpha + beta * x[i]
  }
  
  Sigma = matrix(c(1, 1, 1, 1,2.5,1,1,1,5), 3, 3)
  
  mu1 = c(0, 0,0)
  Sigma1 = Sigma
  shape1 = c(0, 0,0)
  eta1 = 0.5
  omega1 = 1
  nu1 = c(eta1, omega1, omega1)
  nu1.1=c(eta1,2,2)
  
  mu2 = c(0, 0,0)
  Sigma2 = Sigma
  eta2 = -0.5
  omega2 = 2
  nu2 = c(eta2, omega2, omega2)
  shape2 = c(0, 0,0)
  nu2.2=c(eta2,2,2)
  
  
  mu3 = c(0, 0,0)
  Sigma3 = Sigma
  eta3 = 1
  omega3 = 3
  nu3 = c(eta3, omega3, omega3)
  shape3 = c(0, 0,0)
  nu3.3=c(eta3,2,2)
  
  pii = c(0.2,0.3, 0.5)
  
  arg1 = list(mu = mu1, Sigma = Sigma1, shape = shape1, nu = nu1)
  arg2 = list(mu = mu2, Sigma = Sigma2, shape = shape2, nu = nu2)
  arg3 = list(mu = mu3, Sigma = Sigma3, shape = shape3, nu = nu3)
  
  y1 = rmmix(n = n, pii = pii, "GIG", list(arg1, arg2, arg3))
  y = y1 + mu
  nu = list(nu1, nu2, nu3)
  nuini=list(nu1.1,nu2.2,nu3.3)
  #svm_model <- svm(y, kernel = "polynomial",degree=2, type = "C-classification")
  
  
  #predictions <- predict(svm_model, y1)
  #modeln<-smsn.mmix(y, nu=1, mu =NULL, Sigma = NULL, shape = NULL, pii = NULL, g = g, get.init = TRUE, criteria = FALSE,
  #                  group = FALSE, family = "Normal", error = 0.0001, iter.max = 100, uni.Gama = T, calc.im=FALSE,
  #                  obs.prob= FALSE, kmeans.param = NULL)
  
  # Step 1: Cluster the data using K-means to generate 3 clusters
  kmeans_result <- kmeans(y, centers = 3)
  
  # Get the cluster labels generated by K-means
  cluster_labels <- kmeans_result$cluster
  
  # Step 2: Train the SVM model using the clustered labels
  svm_model <- svm(y, as.factor(cluster_labels), type = "C-classification"
                   , kernel = "polynomial",degree = 1,gamma = 0.7)
  
  # Step 3: Use the trained SVM model to classify new data
  # Example of predicting for the same data (you can replace with new data)
  predicted_clusters <- predict(svm_model, y)
  #sort(table(predicted_clusters)/n)
  resini2=aprox.inicial(y,condi=T,g1=3)
  nuini=resini2$nu
  resini=aprox.inicial(y,x,condi=T,equal=F,g1=3,nuini)
  #resini$nu[[3]][2]<-resini2$nu[[3]][2]
  #resini$pii<-sort(modeln$pii)  #rev(table(predictions)/500)
  resini$pii<-sort(table(predicted_clusters)/n)
  #resini$Sigma<-cov(y1)
  resultado[[hh]] =EM(y, x, alpha, beta, Sigma, nu, pii,g=3,iter.max=100,error=0.00001,condi=T, equal=F)
  
  #best_parameters<-grid_search_log_likelihood3nu2fixed(y,x, resini$alpha, resini$beta, 
  #                                                     resini$Sigma, resini$nu, resini$pii, 
  #                                                     grid_size = 0.5)
  #resini$nu[[2]][2]<-best_parameters$best_nu2
  
  #best_parameters<-grid_search_log_likelihood3nu3fixed(y,x, resini$alpha, resini$beta, 
  #                                                     resini$Sigma, resini$nu, resini$pii, 
  #                                                     grid_size = 1.5)
  #resini$nu[[3]][2]<-best_parameters$best_nu3
  
  #best_parameters<-grid_search_log_likelihood3nu1fixed(y,x, resini$alpha, resini$beta, 
  #                                                     resini$Sigma, resini$nu, resini$pii, 
  #                                                     grid_size = 0.8)
  #resini$nu[[1]][2]<-best_parameters$best_nu1
  
  #best_parameters <- grid_search_log_likelihood3nu1nu3fixed(y, x, resini$alpha, resini$beta, 
  #                                                          resini$Sigma, resini$nu, resini$pii, 
  #                                                          grid_size = 0.8) 
  #resini$nu[[2]][2]<-best_parameters$best_nu2
  
  #best_parameters <- grid_search_log_likelihood3(y, x, resini$alpha, resini$beta, 
  #                                              resini$Sigma, resini$nu, resini$pii, 
  #                                              grid_size = 0.5)
  #best_parameters <- grid_search_log_likelihood3(y, x, alpha, beta, 
  #                                               Sigma, nu, pii, 
  #                                               grid_size = 0.5)
  #resini$nu[[1]][2]<-best_parameters$best_nu1
  #resini$nu[[3]][2]<-best_parameters$best_nu3
  
  resultado[[hh]] =EM(y, x, resini$alpha, resini$beta, resini$Sigma, resini$nu
                      , resini$pii,g=3,iter.max=100,error=0.00001,condi=T, equal=F)
  resultado[[hh]]
  
  print(hh)
}
save(resultado,file="resultadon6003misturas.RData")

alphamont=matrix(0,p,mont)
betamont=matrix(0,p,mont)
Sigmamont=c()
Sigmamont2=c()
Sigmamont3=c()
Sigmamont4=c()
Sigmamont5=c()
Sigmamont6=c()
numont1=matrix(0,2,mont)
numont2=matrix(0,2,mont)
numont3=matrix(0,2,mont)
sigmaTT=matrix(0,p,p)
res=resultado
piimont=matrix(0,3,mont)
for (jj in 1:mont){
  alphamont[,jj]=res[[jj]]$alpha
  betamont[,jj]=res[[jj]]$beta
  Sigma=res[[jj]]$Sigma
  Sigmaante=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][1]
  Sigmaante2=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][2]
  Sigmaante3=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][3]
  Sigmaante4=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][4]
  Sigmaante5=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][5]
  Sigmaante6=res[[jj]]$Sigma[lower.tri(Sigma,diag = T)][6]
  Sigmamont[jj]=Sigmaante
  Sigmamont2[jj]=Sigmaante2
  Sigmamont3[jj]=Sigmaante3
  Sigmamont4[jj]=Sigmaante4
  Sigmamont5[jj]=Sigmaante5
  Sigmamont6[jj]=Sigmaante6
  numont1[,jj]=res[[jj]]$nu[[1]]
  numont2[,jj]=res[[jj]]$nu[[2]]
  numont3[,jj]=res[[jj]]$nu[[3]]
  sigmaTT=res[[jj]]$Sigma+sigmaTT
  piimont[,jj]=res[[jj]]$pii
}
#save(res,file ="resn500nu9.RData")
#save(erropad,file="erropadn500nu9.RData")
#save(tt,file="tt500nu9.RData")
library(xtable)
xtable(cbind(
  c(
    apply(numont1, 1, mean),
    apply(numont2, 1, mean),
    apply(numont3, 1, mean),
    apply(alphamont, 1, mean),
    apply(betamont, 1, mean),
    mean(Sigmamont),
    mean(Sigmamont2),
    mean(Sigmamont3),
    mean(Sigmamont4),
    mean(Sigmamont5),
    mean(Sigmamont6),
    apply(piimont, 1, mean)
  ),
  c(
    apply(numont1, 1, median),
    apply(numont2, 1, median),
    apply(numont3, 1, median),
    apply(alphamont, 1, median),
    apply(betamont, 1, median),
    median(Sigmamont),
    median(Sigmamont2),
    median(Sigmamont3),
    median(Sigmamont4),
    median(Sigmamont5),
    median(Sigmamont6),
    apply(piimont, 1, median)
  ),
  c(
    apply(numont1, 1, sd),
    apply(numont2, 1, sd),
    apply(numont3, 1, sd),
    apply(alphamont, 1, sd),
    apply(betamont, 1, sd),
    sd(Sigmamont),
    sd(Sigmamont2),
    sd(Sigmamont3),
    sd(Sigmamont4),
    sd(Sigmamont5),
    sd(Sigmamont6),
    apply(piimont, 1, sd)
  )
),digits=4)

#############real data galea
# Load necessary libraries
rm(list=ls(all=TRUE))
library(readr)
library(dplyr)


#install.packages('ghyp')
#install.packages('expm')
#install.packages('mvtnorm')
#install.packages('mixsmsn')
require(ghyp)
require(expm)
require(mvtnorm)
require(mixsmsn)
#/Users/davidesteban/Documents/David/Tesis Mestrado
#/Users/davidesteban/Downloads
#### mudar o diretório de trabalho ###########
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/gera2.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/densidades.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/capm.txt")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/capmini.txt")

# Import the data (assuming it's in a CSV file named 'chile_stock_data.csv')
data <- readxl::read_xlsx("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/BaseDatosPaper2020.xlsx")

convert_to_numeric <- function(x) {
  as.numeric(gsub("%", "", x)) 
}

data <- data %>%
  mutate(across(-PERÍODO, convert_to_numeric))


# View the data frame with calculated log returns
# Convert percentage strings to numeric by removing '%' and dividing by 100

# Calculate excess returns for the market (market - risk-free rate) and individual assets


data <- data %>%
  mutate(Market_Excess = IPSA - TIPROM,
         SQMB_Excess = SQMB - TIPROM,
         ENEL_Excess = ENEL - TIPROM,
         FALABELLA_Excess = FALABELLA - TIPROM,
         BSANTANDER_Excess = BSANTANDER - TIPROM,
         LTM_Excess = LTM - TIPROM)

#y=data$SQMB_Excess
#x=data$Market_Excess
library(e1071)
g1=g=2
y1=as.matrix(cbind(data$SQMB_Excess,data$ENEL_Excess,data$FALABELLA_Excess
                   ,data$BSANTANDER_Excess,data$LTM_Excess))
y1<-y1[-77,]
y1=y1[,-4]
y=y1
x=data$Market_Excess[-77]
n =dim(y)[1]
p =dim(y)[2]
lb=matrix(rep(0.1,p),p,1)
lu=matrix(rep(10,p),p,1)
error=0.00001
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/tcapm.txt")
tcapm<-emheltonnu(y1,x,rep(1,5),rep(1,5),error,lb,lu,nu=1,dist="Student")
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/envelot.txt")
envelopes(y1,tcapm$alpha,tcapm$beta,tcapm$Sigma,tcapm$nu,"Student")

apply(y, 2, kurtosis)

capm<-function(y,x,g1,c,e){
  n =dim(y)[1]
  p =dim(y)[2]  
modelt<-smsn.mmix(y, nu=25, mu =NULL, Sigma = NULL, shape = NULL, pii = NULL, g = g1, get.init = TRUE, criteria = FALSE,
                  group = FALSE, family =  "t", error = 0.0001, iter.max = 100, uni.Gama = T, calc.im=FALSE,
                  obs.prob= FALSE, kmeans.param = NULL)
if(p==1){
modelt<-smsn.mix(y, nu=100, mu =NULL, shape = NULL, pii = NULL, g = g1, get.init = TRUE, criteria = FALSE,
                  group = FALSE, family =  "t", error = 0.00001, iter.max = 100, calc.im=FALSE,
                  obs.prob= FALSE, kmeans.param = NULL)}
nu3=rep(sqrt(modelt$nu),3)
nuini <- rep(list(nu3), g1)
nuentrada <- nu.ini <- nuini

resini=aprox.inicial(y,x,condi=T,equal=F,g1,nuini)
#3,0.7, 0.9
#1, 0.4
svm_model <- svm(y, kernel = "polynomial",degree=1,gamma = 0.3)
# Display the model summary  
predictions <- predict(svm_model, y)
resini$pii<-(rev(table(predictions)/n))
#resini$pii<-modelt$pii
best_parameters <- grid_search_log_likelihood(y, x, resini$alpha, resini$beta, 
                                              resini$Sigma, resini$nu, resini$pii, 
                                              grid_size = 0.3)

#grid_search_log_likelihood_nu2(y, x, resini$alpha, resini$beta, 
#                               resini$Sigma, resini$nu, resini$pii, 
#                               grid_size = 0.3)
resini$nu[[1]][2]<-best_parameters$best_nu1
resini$nu[[2]][2]<-best_parameters$best_nu2
####
best_parameters <- grid_search_log_likelihoodeta(y, x, resini$alpha, resini$beta, 
                                              resini$Sigma, resini$nu, resini$pii, 
                                              grid_size = 1.2)

#grid_search_log_likelihood_nu2(y, x, resini$alpha, resini$beta, 
#                               resini$Sigma, resini$nu, resini$pii, 
#                               grid_size = 0.3)
resini$nu[[1]][1]<-best_parameters$best_nu1
resini$nu[[2]][1]<-best_parameters$best_nu2
resultado=EM(y, x, resini$alpha, resini$beta, resini$Sigma, resini$nu
                    , resini$pii,g=g1,iter.max=200,error=0.00001,condi=c, equal=e)
return(resultado)
}
# Run CAPM model for each asset using the market excess return as the independent variable

capm_sqmb <-capm(y1,x,2,T,F)
#capm_sqmb3 <-capm(y1,x,3,T,F)
BICcapm<-function(y,model,k1){
  n=dim(as.matrix(y))[1]
  k=2*p+(p*(p-1))/2+k1
  Lk<- -model$lk
  BIC <- 2 * (Lk) + k * log(n)
  AIC <- 2*k+2*Lk
  HQ <- 2* (Lk)+2*k*log(log(n))
  return(rbind(-Lk,BIC,AIC,HQ))
}
rbind(cbind(BICcapm(y,capm_sqmb,3),BICcapm(y,tcapm,1)))
# Create scatter plots with regression lines for each CAPM model
plot_capm <- function(data, excess_return, market_return, model, model2, nu, asset_name) {
  # Extract coefficients for intercept and slope
  intercept <- model$alpha[nu,1]
  slope <- model$beta[nu,1]
  intercept2 <- model2$alpha[nu]
  slope2 <- model2$beta[nu]
  
  # Dummy data for legend
  legend_data <- data.frame(
    type = c("SMNGIG", "t"),
    color = c("darkblue", "red"),
    intercept = c(intercept, intercept2),
    slope = c(slope, slope2)
  )
  
  ggplot(data, aes_string(x = market_return, y = excess_return)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_abline(aes(intercept = intercept, slope = slope, color = "t"), data = legend_data, show.legend = TRUE) +
    geom_abline(aes(intercept = intercept2, slope = slope2, color = "SMNGIG"), data = legend_data, show.legend = TRUE) +
    scale_color_manual(
      name = "", 
      values = c("SMNGIG" = "darkblue", "t" = "red"),
      labels = c("SMNGIG", "t")
    ) +
    ggtitle(paste("CAPM Model for", asset_name)) +
    xlab("Market Excess Return") +
    ylab(paste(asset_name, "Excess Return")) +
    theme_minimal() +
    theme(
      legend.position = "top",  # Optional: Adjust legend position
      
      axis.title = element_text(size = 16),  # Increase axis titles size
      axis.text = element_text(size = 16),   # Increase axis tick labels size
      legend.text = element_text(size = 12),  # Increase legend text size
      plot.title = element_text(size = 18)#, hjust = 0.5)
    )+
    theme(legend.position = "top")#"top"  # Optional: Adjust legend position
}

# In this code, the `legend_data` creates a custom legend without tying it to the main data.

# Generate and display the plots
plot_sqmb <- plot_capm(data[-77,], "SQMB_Excess", "Market_Excess", capm_sqmb,tcapm,1, "SQMB")
plot_enel <- plot_capm(data[-77,], "ENEL_Excess", "Market_Excess", capm_sqmb,tcapm,2, "ENEL")
plot_falabella <- plot_capm(data[-77,], "FALABELLA_Excess", "Market_Excess", capm_sqmb,tcapm,3, "FALABELLA")
plot_bSantander <- plot_capm(data[-77,], "BSANTANDER_Excess", "Market_Excess", capm_sqmb,tcapm,4, "BSANTANDER")
plot_ltm <- plot_capm(data[-77,], "LTM_Excess", "Market_Excess", capm_sqmb,tcapm,5, "LTM")

# Print plots
print(plot_sqmb)
print(plot_enel)
print(plot_falabella)
print(plot_bSantander)
print(plot_ltm)






p
source("/Users/davidesteban/Documents/David/Tesis Mestrado/CAPM/envelopemixgig.txt")
model=capm_sqmb
pii=capm_sqmb$pii
alpha=model$alpha
beta=model$beta
Sigma=model$Sigma
pii=model$pii
eta1=model$nu[[1]][1]
eta2=model$nu[[2]][1]
eta3=model$nu[[3]][1]
omega1=model$nu[[1]][2]
omega2=model$nu[[2]][2]
omega3=model$nu[[3]][2]
nu1 = c(eta1, omega1, omega1)
nu2 = c(eta2, omega2, omega2)
nu2 = c(eta3, omega3, omega3)
nu = list(nu1, nu2, nu3)
envelopesempriricadist(y1,x,alpha=model$alpha,beta=model$beta,Sigma=model$Sigma
                       ,pii=model$pii,eta1=model$nu[[1]][1],eta2=model$nu[[2]][1]
                       ,eta3=model$nu[[3]][1],omega1=model$nu[[1]][2],omega2=model$nu[[2]][2],
                       omega3=model$nu[[3]][2])
round(model$Sigma,4)
round(tcapm$Sigma,4)



#######
library(boot)
alpha_estimates=matrix(0,ncol=1000,nrow=10)
beta_estimates=matrix(0,ncol=1000,nrow=10)
for(i in 1:1000){
bootstrap_sample <-apply(y1,2,sample, size =n, replace = TRUE)
bootstrap_sample <- y1[sample(1:nrow(y1), size = nrow(y1), replace = TRUE), ]
mixgig<-capm(bootstrap_sample,x,g1,T,F)
tcap<-emheltonnu(bootstrap_sample,x,rep(1,5),rep(1,5),error,lb,lu,nu=1,dist="Student")
alpha_estimates[,i]=c(mixgig$alpha,tcap$alpha)
beta_estimates[,i]=c(mixgig$beta,tcap$beta)
}
round(apply(alpha_estimates,1,sd),4)
round(apply(beta_estimates,1,sd),4)
i
