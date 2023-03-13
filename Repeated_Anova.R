library(vctrs)
library(xlsx)
library(car)
library(bruceR)
#By Aquilus, required packages as above
#import data (Note this code is suitable for long data, if your data is in wide
#form, arrange it by your factor column e.g. 'group' or 'condition' or 'treatment' et al.)
data<-read.xlsx(file='Beta_regress.xls',sheetIndex=1)
data2<-data[,c(1,2,11,14)]
#create an subject ID vector, ignore if existed already
names(data2)[1]<-'ID'
ID<-matrix(0,c(80,1))
for (i in 1:20)
{ID[i*4-3,]<-i
ID[i*4-2,]<-i
ID[i*4-1,]<-i
ID[i*4,]<-i}
data2$ID<-factor(ID,ordered=F)
# Normality test: this chunk will perform shapiro wilk test for each combination 
#(condition:group), rename your corresponding columns as 'condition' and 'group'(recommended)
# or change input in chunck (less convenient)
condition<-c('base','mild','moderate','recovery')
group<-c('0','1')
ntest<-matrix(0,nrow=2,ncol=4)
colnames(ntest)<-c('base','mild','moderate','recovery')
for (i in 1:2){for (j in 1:4)
{d<-shapiro.test(data2$HB_beta[data2$condition==condition[j]&data2$Group==group[i]])
  ntest[i,j]<-d$p.value}}
ntest<-as.data.frame(ntest)   #ntest stores results of shapiro test
colnames(ntest)<-condition
rownames(ntest)<-c('responsive','drowsy')
#write out
write.table(ntest, file = "Anovaresult.txt", append = FALSE,sep = " ", col.names = TRUE)

# Levene test
ltest<-leveneTest(HB_beta~interaction(Group, condition), data2)# for considering all 8 groups and center set as default (median)
#write out
sink('Anovaresult.txt',append=TRUE)
leveneTest(HB_beta~interaction(Group, condition), data2)
sink(NULL)
# Anova based on bruceR$MANOVA
ano<-MANOVA(data=data2, dv = 'HB_beta', subID = 'ID', within = 'condition', between='Group',sph.correction="GG") # you can specify covariates with 'c(cv1, cv2,cv3,...)'
# write out
sink('Anovaresult.txt',append=TRUE)
MANOVA(data=data2, dv = 'HB_beta', subID = 'ID', within = 'condition', between='Group',sph.correction="GG")
sink(NULL)
# Simple effect and post hoc based on bruceR$EMEANS
sink('Anovaresult.txt',append=TRUE)
EM1<-EMMEANS(ano,effect='Group',by='condition',p.adjust='none')
EM2<-EMMEANS(ano,effect='condition',by='Group',p.adjust='tukey')
EM3<-EMMEANS(ano,effect='condition',p.adjust='tukey')
sink(NULL)