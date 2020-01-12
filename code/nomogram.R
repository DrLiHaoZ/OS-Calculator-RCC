library(rms)
library(ggplot2)
training <- read.csv("D:/Article/Project/last-project/training.csv")

dd=datadist(training)
options(datadist="dd")


f <- cph(Surv(survival,dead) ~ Age+Marital+Race+Histology+Grade+TumorSize+
         NodeInvolvement+Metastasis+Surgery+Radiation+Chemotherapy, 
         data = training,x = TRUE,y=TRUE,surv = TRUE)
surv = Survival(f)
surv1 = function(x)surv(12,x)
surv2 = function(x)surv(36,x)
nom = nomogram(f,fun=list(surv1,surv2),funlabel = c("1-Year Survival","3-Year Survival"))
plot(nom,cex.axis = 0.5,cex.var = 0.8)
#¼ÆËãc-index
OStraining <- read.csv("D:/Article/Project/last-project/OStraining.csv")
OSvalidation <- read.csv("D:/Article/Project/last-project/OSvalidation.csv")
training <- read.csv("D:/Article/Project/One-in-all/2019-6-29-training.csv")
validation <- read.csv("D:/Article/Project/One-in-all/2019-6-29-validation.csv")

dd=datadist(OStraining)
options(datadist="dd")
f <- cph(Surv(survival,dead) ~ total_score, 
         data = OStraining,x = TRUE,y=TRUE,surv = TRUE)
coxpe <- predict(f)
c_index=1-rcorr.cens(coxpe,Surv(OStraining$survival,OStraining$dead))

dd=datadist(OSvalidation)
options(datadist="dd")
f <- cph(Surv(survival,dead) ~ total_score, 
         data = OSvalidation,x = TRUE,y=TRUE,surv = TRUE)
coxpe <- predict(f)
c_index=1-rcorr.cens(coxpe,Surv(OSvalidation$survival,OSvalidation$dead))

dd=datadist(training)
options(datadist="dd")
f <- cph(Surv(survival,dead) ~ t+n+m, 
         data = training,x = TRUE,y=TRUE,surv = TRUE)
coxpe <- predict(f)
c_index=1-rcorr.cens(coxpe,Surv(training$survival,training$dead))


dd=datadist(validation)
options(datadist="dd")
f <- cph(Surv(survival,dead) ~ t+n+m, 
         data = validation,x = TRUE,y=TRUE,surv = TRUE)
coxpe <- predict(f)
c_index=1-rcorr.cens(coxpe,Surv(validation$survival,validation$dead))

library(survival)
f_nomo_tr <- coxph(Surv(survival,dead) ~ total_score,data = OStraining)
f_nomo_vl <- coxph(Surv(survival,dead) ~ total_score,data = OSvalidation)
f_tnm_tr <- coxph(Surv(survival,dead) ~ t+n+m,data = training)
f_tnm_vl <- coxph(Surv(survival,dead) ~ t+n+m,data = validation)

