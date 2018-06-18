m<-read.table("C:\\Users\\Satabdi\\Desktop\\R\\voice\\b.csv",header=TRUE,sep=",")
m
summary(m)
names(m)
is.na(m)

png(file="boxplot.png")
boxplot(m,las=2,names=c("name","MDVP.Fo.Hz.","MDVP.Fhi.Hz.","MDVP.Flo.Hz.","MDVP.Jitter...","MDVP.Jitter.Abs.","MDVP.RAP","MDVP.PPQ","Jitter.DDP","MDVP.Shimmer","MDVP.Shimmer.dB.","Shimmer.APQ3","Shimmer.APQ5","MDVP.APQ","Shimmer.DDA","NHR","HNR","status","RPDE","DFA","spread1","spread2","D2","PPE"))
dev.off()

outlier_values<- boxplot.stats(m$MDVP.Fhi.Hz)$out
boxplot(m$MDVP.Fhi.Hz, main="MDVP.Fhi.Hz", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

str(m)
summary(m)
m_sub<-subset(m,MDVP.Fo.Hz. <210 &MDVP.Fhi.Hz. <300 &MDVP.Flo.Hz. <160 )
summary(m_sub)
str(m_sub)
png(file="boxplot.png")
boxplot(m_sub,las=2,names=c("name","MDVP.Fo.Hz.","MDVP.Fhi.Hz.","MDVP.Flo.Hz.","MDVP.Jitter...","MDVP.Jitter.Abs.","MDVP.RAP","MDVP.PPQ","Jitter.DDP","MDVP.Shimmer","MDVP.Shimmer.dB.","Shimmer.APQ3","Shimmer.APQ5","MDVP.APQ","Shimmer.DDA","NHR","HNR","status","RPDE","DFA","spread1","spread2","D2","PPE"))
dev.off()

library(MASS)
library(neuralnet)
str(m_sub)
m_sub$MDVP.Fo.Hz.<-(m_sub$MDVP.Fo.Hz.-min(m_sub$MDVP.Fo.Hz.)/max(m_sub$MDVP.Fo.Hz.)-min(m_sub$MDVP.Fo.Hz.))
m_sub$MDVP.Fhi.Hz.<-(m_sub$MDVP.Fhi.Hz.-min(m_sub$MDVP.Fhi.Hz.)/max(m_sub$MDVP.Fhi.Hz.)-min(m_sub$MDVP.Fhi.Hz.))
m_sub$MDVP.Flo.Hz.<-(m_sub$MDVP.Flo.Hz.-min(m_sub$MDVP.Flo.Hz.)/max(m_sub$MDVP.Flo.Hz.)-min(m_sub$MDVP.Flo.Hz.))
m_sub$HNR<-(m_sub$HNR-min(m_sub$HNR)/max(m_sub$HNR)-min(m_sub$HNR))
m_sub$spread1<-(m_sub$spread1-min(m_sub$spread1)/max(m_sub$spread1)-min(m_sub$spread1))
m_sub$PPE<-(m_sub$PPE-min(m_sub$PPE)/max(m_sub$PPE)-min(m_sub$PPE))


ind<-sample(2,nrow(m_sub),replace=TRUE,prob=c(0.7,0.3))
training<-m_sub[ind==1,]
test<-m_sub[ind==2,]


library(neuralnet)
n<-neuralnet(status~MDVP.Fo.Hz.+MDVP.Fhi.Hz.+MDVP.Flo.Hz.+MDVP.Jitter...+MDVP.Jitter.Abs.+MDVP.RAP+MDVP.PPQ+Jitter.DDP+MDVP.Shimmer+MDVP.Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+MDVP.APQ+Shimmer.DDA+NHR+HNR+RPDE+DFA+spread1+spread2+D2+PPE,data=training,hidden=3,err.fct="ce",linear.output=FALSE)
plot(n)


misclassification(status, m, type = c("exposure", "outcome"),
  bias_parms = NULL, alpha = 0.05)


tab <- table(yp,yt)
 tab 
1-sum(diag(tab))/sum(tab) 

fit <- lm(status~MDVP.Fo.Hz.+MDVP.Fhi.Hz.+MDVP.Flo.Hz.+MDVP.Jitter...+MDVP.Jitter.Abs.+MDVP.RAP+MDVP.PPQ+Jitter.DDP+MDVP.Shimmer+MDVP.Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+MDVP.APQ+Shimmer.DDA+NHR+HNR+RPDE+DFA+spread1+spread2+D2+PPEe, data=m::fuel.frame)
predict(fit)
