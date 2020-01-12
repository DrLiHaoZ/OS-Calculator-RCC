library(rms)
library(survivalROC)
library(ggplot2)
OStraining <- read.csv("D:/Article/Project/last-project/OStraining.csv")
OSvalidation <- read.csv("D:/Article/Project/last-project/OSvalidation.csv")
risk_score_training <- read.delim("D:/Article/Project/One-in-all/risk_score_training.txt")
risk_score_validation <- read.delim("D:/Article/Project/One-in-all/risk_score_validation.txt")
risk_score_training_tnm <- read.delim("D:/Article/Project/One-in-all/risk_score_training_tnm.txt")
risk_score_validation_tnm <- read.delim("D:/Article/Project/One-in-all/risk_score_validation_tnm.txt")


ROC_1_tr = survivalROC(Stime = OStraining$survival,status = OStraining$dead,
                       marker = OStraining$total_score,predict.time = 12,method = "KM")
ROC_3_tr = survivalROC(Stime = OStraining$survival,status = OStraining$dead,
                       marker = OStraining$total_score,predict.time = 36,method = "KM")
ROC_1_vl = survivalROC(Stime = OSvalidation$survival,status = OSvalidation$dead,
                       marker = OSvalidation$total_score,predict.time = 12,method = "KM")
ROC_3_vl = survivalROC(Stime = OSvalidation$survival,status = OSvalidation$dead,
                       marker = OSvalidation$total_score,predict.time = 36,method = "KM")

if(FALSE){
ROC_1_tr = survivalROC(Stime = risk_score_training$survival,status = risk_score_training$dead,
                           marker = risk_score_training$risk_score,predict.time = 12,method = "KM")
ROC_3_tr = survivalROC(Stime = risk_score_training$survival,status = risk_score_training$dead,
                           marker = risk_score_training$risk_score,predict.time = 36,method = "KM")
ROC_1_vl = survivalROC(Stime = risk_score_validation$survival,status = risk_score_validation$dead,
                           marker = risk_score_validation$risk_score,predict.time = 12,method = "KM")
ROC_3_vl = survivalROC(Stime = risk_score_validation$survival,status = risk_score_validation$dead,
                           marker = risk_score_validation$risk_score,predict.time = 36,method = "KM")
}

ROC_1_tnm_tr = survivalROC(Stime = risk_score_training_tnm$survival,status = risk_score_training_tnm$dead,
                       marker = risk_score_training_tnm$risk_score,predict.time = 12,method = "KM")
ROC_3_tnm_tr = survivalROC(Stime = risk_score_training_tnm$survival,status = risk_score_training_tnm$dead,
                           marker = risk_score_training_tnm$risk_score,predict.time = 36,method = "KM")
ROC_1_tnm_vl = survivalROC(Stime = risk_score_validation_tnm$survival,status = risk_score_validation_tnm$dead,
                           marker = risk_score_validation_tnm$risk_score,predict.time = 12,method = "KM")
ROC_3_tnm_vl = survivalROC(Stime = risk_score_validation_tnm$survival,status = risk_score_validation_tnm$dead,
                           marker = risk_score_validation_tnm$risk_score,predict.time = 36,method = "KM")

df_nomo_tr_1 = data.frame(fp = ROC_1_tr$FP,tp = ROC_1_tr$TP,group = "Nomogram")
df_nomo_tr_3 = data.frame(fp = ROC_3_tr$FP,tp = ROC_3_tr$TP,group = "Nomogram")
df_nomo_vl_1 = data.frame(fp = ROC_1_vl$FP,tp = ROC_1_vl$TP,group = "Nomogram")
df_nomo_vl_3 = data.frame(fp = ROC_3_vl$FP,tp = ROC_3_vl$TP,group = "Nomogram")
df_tnm_tr_1 = data.frame(fp = ROC_1_tnm_tr$FP,tp = ROC_1_tnm_tr$TP,group = "AJCC/TNM")
df_tnm_tr_3 = data.frame(fp = ROC_3_tnm_tr$FP,tp = ROC_3_tnm_tr$TP,group = "AJCC/TNM")
df_tnm_vl_1 = data.frame(fp = ROC_1_tnm_vl$FP,tp = ROC_1_tnm_vl$TP,group = "AJCC/TNM")
df_tnm_vl_3 = data.frame(fp = ROC_3_tnm_vl$FP,tp = ROC_3_tnm_vl$TP,group = "AJCC/TNM")

pdf("C:\\Users\\李浩\\Desktop\\p_1_vl.pdf",width = 4,height = 4)
df_1_vl = rbind(df_nomo_vl_1,df_tnm_vl_1)
ggplot(data = df_1_vl,mapping = aes(x = fp,y = tp,group = group,color = group))+
  geom_line(size = 1)+xlab("False Positive Rate")+ylab("True Positive Rate")+
  geom_abline(slope = 1,intercept = 0,color = "white")+
  theme(legend.position =  c(0.8,0.2),legend.background = element_blank(),legend.key = element_blank())+
  labs(color = "Model")+
  annotate(geom = "text",label = paste("AUC:", round(ROC_1_vl$AUC,3)),x = 0.15,y = 0.9)+
  annotate(geom = "text",label = paste("AUC:", round(ROC_1_tnm_vl$AUC,3)),x = 0.35,y = 0.6)
dev.off()

pdf("C:\\Users\\李浩\\Desktop\\p_3_vl.pdf",,width = 4,height = 4)
df_3_vl = rbind(df_nomo_vl_3,df_tnm_vl_3)
ggplot(data = df_3_vl,mapping = aes(x = fp,y = tp,group = group,color = group))+
  geom_line(size = 1)+xlab("False Positive Rate")+ylab("True Positive Rate")+
  geom_abline(slope = 1,intercept = 0,color = "white")+
  theme(legend.position =  c(0.8,0.2),legend.background = element_blank(),legend.key = element_blank())+
  labs(color = "Model")+
  annotate(geom = "text",label = paste("AUC:", round(ROC_3_vl$AUC,3)),x = 0.15,y = 0.9)+
  annotate(geom = "text",label = paste("AUC:", round(ROC_3_tnm_vl$AUC,3)),x = 0.35,y = 0.6)
dev.off()

pdf("C:\\Users\\李浩\\Desktop\\p_1_tr.pdf",,width = 4,height = 4)
df_1_tr = rbind(df_nomo_tr_1,df_tnm_tr_1)
ggplot(data = df_1_tr,mapping = aes(x = fp,y = tp,group = group,color = group))+
  geom_line(size = 1)+xlab("False Positive Rate")+ylab("True Positive Rate")+
  geom_abline(slope = 1,intercept = 0,color = "white")+
  theme(legend.position =  c(0.8,0.2),legend.background = element_blank(),legend.key = element_blank())+
  labs(color = "Model")+
  annotate(geom = "text",label = paste("AUC:", round(ROC_1_tr$AUC,3)),x = 0.15,y = 0.9)+
  annotate(geom = "text",label = paste("AUC:", round(ROC_1_tnm_tr$AUC,3)),x = 0.35,y = 0.6)
dev.off()

pdf("C:\\Users\\李浩\\Desktop\\p_3_tr.pdf",width = 4,height = 4)
df_3_tr = rbind(df_nomo_tr_3,df_tnm_tr_3)
ggplot(data = df_3_tr,mapping = aes(x = fp,y = tp,group = group,color = group))+
  geom_line(size = 1)+xlab("False Positive Rate")+ylab("True Positive Rate")+
  geom_abline(slope = 1,intercept = 0,color = "white")+
  theme(legend.position =  c(0.8,0.2),legend.background = element_blank(),legend.key = element_blank())+
  labs(color = "Model")+
  annotate(geom = "text",label = paste("AUC:", round(ROC_3_tr$AUC,3)),x = 0.15,y = 0.9)+
  annotate(geom = "text",label = paste("AUC:", round(ROC_3_tnm_tr$AUC,3)),x = 0.35,y = 0.6)
dev.off()
