library(rms)
library(ggplot2)

OStraining <- read.csv("D:/Article/Project/last-project/OStraining.csv")
OSvalidation <- read.csv("D:/Article/Project/last-project/OSvalidation.csv")

B = 100

dd=datadist(OStraining)
options(datadist="dd")
f <- cph(Surv(survival,dead) ~  total_score, data = OStraining,x = TRUE,y=TRUE,surv = TRUE,time.inc = 1*12)
cal_1_tr <- calibrate(f, cmethod="KM", method="boot",u=1*12*1, m= 3000,B=B)
f <- cph(Surv(survival,dead) ~  total_score, data = OStraining,x = TRUE,y=TRUE,surv = TRUE,time.inc = 3*12)
cal_3_tr <- calibrate(f, cmethod="KM", method="boot",u=3*12*1, m= 3000,B=B)

df_tr_1 = data.frame(pre = cal_1_tr[,'mean.predicted'],act = cal_1_tr[,'KM'],
                   err = cal_1_tr[,'std.err'],Group = "1-Year")
df_tr_3 = data.frame(pre = cal_3_tr[,'mean.predicted'],act = cal_3_tr[,'KM'],
                   err = cal_3_tr[,'std.err'],Group = "3-Year")
df_tr = rbind(df_tr_1,df_tr_3)

pdf("C:\\Users\\ÀîºÆ\\Desktop\\cali_tr.pdf",width = 4,height = 4)
ggplot()+
  geom_abline(slope = 1,intercept = 0,color = "white",size = 1)+
  geom_line(data = df_tr,aes(x = pre,y = act,group = Group,color = Group),size = 1.2)+
  geom_errorbar(df_tr,mapping = aes(x = pre,ymin = act-err,ymax = act+err,color = Group),width = 0.04,size = 0.2)+
  geom_point(df_tr,mapping = aes(x = pre,y = act,group = Group,color = Group),size = 2,shape = 21,fill = "white")+
  labs(x = "Nomogram-Predicted OS",y = "Actual OS")+
  theme(legend.position =  c(0.8,0.2),legend.background = element_blank())+
  xlim(0,1.01)
dev.off()
  
  

dd=datadist(OSvalidation)
options(datadist="dd")
f <- cph(Surv(survival,dead) ~  total_score, data = OSvalidation,x = TRUE,y=TRUE,surv = TRUE,time.inc = 1*12)
cal_1_vl <- calibrate(f, cmethod="KM", method="boot",u=1*12*1, m= 800,B=B)
f <- cph(Surv(survival,dead) ~  total_score, data = OSvalidation,x = TRUE,y=TRUE,surv = TRUE,time.inc = 3*12)
cal_3_vl <- calibrate(f, cmethod="KM", method="boot",u=3*12*1, m= 800,B=B)

df_vl_1 = data.frame(pre = cal_1_vl[,'mean.predicted'],act = cal_1_vl[,'KM'],
                     err = cal_1_vl[,'std.err'],Group = "1-Year")
df_vl_3 = data.frame(pre = cal_3_vl[,'mean.predicted'],act = cal_3_vl[,'KM'],
                     err = cal_3_vl[,'std.err'],Group = "3-Year")
df_vl = rbind(df_vl_1,df_vl_3)


pdf("C:\\Users\\ÀîºÆ\\Desktop\\cali_vl.pdf",width = 4,height = 4)
ggplot()+
  geom_abline(slope = 1,intercept = 0,color = "white",size = 1)+
  geom_line(data = df_vl,aes(x = pre,y = act,group = Group,color = Group),size = 1.2)+
  geom_errorbar(df_vl,mapping = aes(x = pre,ymin = act-err,ymax = act+err,color = Group),width = 0.04,size = 0.2)+
  geom_point(df_vl,mapping = aes(x = pre,y = act,group = Group,color = Group),size = 2,shape = 21,fill = "white")+
  labs(x = "Nomogram-Predicted OS",y = "Actual OS")+
  theme(legend.position =  c(0.8,0.2),legend.background = element_blank())+
  xlim(0,1.01)
dev.off()


