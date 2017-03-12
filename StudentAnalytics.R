#Data Cleaning
AttendanceData<-read.csv("Cabarrus_Attendance.csv", header=TRUE);
ClearingHouseData <- read.csv("Cabarrus_Clearinghouse.csv",header=TRUE);
DemographicsData <- read.csv("Cabarrus_Demographics.csc",header=TRUE);
MarksData <- read.csv("Cabarrus_Marks_Grades.csv", header = TRUE); 
MarksData$Subject<-gsub("Mathmatics","Mathematics",MarksData$Subject);
MarksData$Value[MarksData$Value > 100] <- 100
MarksData$Course.Title[MarksData$Course.Title=="SECOND"]<-"ENGLISH AS A SECOND LANGUAGE"
MarksData$Subject[ MarksData$Course.Title %in% "science"]<-"Science" 
if(MarksData$Course.Title %in% c ("SCIENCE")) MarksData$Subject <-"Science" 
if(grep("SCIENCE", MarksData$Course.Title))MarksData$Subject< -"Science" 
if(grep("CCP-HIS111 HUMANITIES & SOCIAL SCIENCES", MarksData$Course.Title )) MarksData$Subject<- "Social Science"
if(grep ("AMERICAN HISTORY" , MarksData$Course.Title)) MarksData$Subject<-"Social Science"
if(grep ("SOCIAL", MarksData$Course.Title)) MarksData$Subject<-"Social Science" 
if(grep("MATHEMATICS", MarksData$Course.Title)) MarksData$Subject<-"Mathematics"
if(grep("ENGLISH", MarksData$Course.Title )) MarksData$Subject<-"English" 
if(grep("SECD", MarksData$Course.Title)) MarksData$Course.Title<-"SECOND"
MarksData$Course.Title [MarksData$Section.Number==70155|MarksData$Section.Number==70156]<-"ENGLISH AS A SECOND LANGUAGE BEGINNER" 
MarksData$Course.Title [MarksData$Section.Number==70158|MarksData$Section.Number==70157]<-"ENGLISH AS A SECOND LANGUAGE INTERMEDIATE"
write.csv (MarksData, "C:/Masters in UNCC/Spring 2016 Semester/Knowledge Discovery in Databases/KDD Project/MarksData.csv", sep=",")
MarksData$Subject[str_detect (MarksData$Course.Title,"CCP-HIS111 HUMANITIES & SOCIAL SCIENCES")]<-"Social Science" 
MarksData$Subject[str_detect (MarksData$Course.Title,"MATHEMATICS")]<-"Mathematics" 
MarksData $Subject[str_detect(MarksData$Course.Title,"SOCIAL")]<-"Social Science" 
MarksData $Subject[str_detect(MarksData$Course.Title,"ENGLISH")]<-"English"
MarksData$Subject[nchar(as.character (MarksData$Subject))==0] <-NA 
setnames(MarksData,old = c("Value"), new=c("Marks_and_Grades"))



#Correlation

GraduationData<-read.csv("GraduationData.csv")
AbsencesModel<-glm(graduated~Grade9_Sumoftotal_absences+Grade10_Sum.of.total_absences+Grade11_Sum.of.total_absences+Grade12_Sum.of.total_absences, data=GraduationData, family=binomial()) 
AbsencesModel_Chi<- AbsencesModel$null.deviance-AbsencesModel$deviance 
AbsencesModel_DF<-AbsencesModel$df.null-AbsencesModel$df.residual 
AbsencesModel_ChiSquareTest<- 1-pchisq(AbsencesModel_Chi,AbsencesModel_DF) AbsencesModel_ChiSquareTest
exp(AbsencesModel$coefficients)

MarksModel<-glm(graduated~Grade9_AverageMarks+Grade10_Average.Marks+Grade11_Average.Marks+ Grade12_Average.Marks, data=GraduationData, family=binomial())
MarksModel_Chi<-MarksModel$null.deviance-MarksModel$deviance
MarksModel_DF<-MarksModel$df.null-MarksModel$df.residual
MarksModel_ChiSquareTest<- 1-pchisq(MarksModel_Chi,MarksModel_DF) 
exp(MarksModel$coefficients)



UnexcusedAbsencesModel<-glm(graduated~Grade9_Sum_ofUnexcused_absences+Grade10_Sum.of.Unexcused_absence s+Grade11_Sum.of.Unexcused_absences+Grade12_Sum.of.Unexcused_absences, data=GraduationData, family=binomial())
UnexcusedAbsencesModel_Chi<-UnexcusedAbsencesModel$null.deviance-UnexcusedAbsencesModel$deviance 
UnexcusedAbsencesModel_DF<-UnexcusedAbsencesModel$df.null-UnexcusedAbsencesModel$df.residual
UnexcusedAbsencesModel_ChiSquareTest<- 1-pchisq(UnexcusedAbsencesModel_Chi,UnexcusedAbsencesModel_DF)
UnexcusedAbsencesModel_ChiSquareTest
exp(UnexcusedAbsencesModel$coefficients)


TotalAbsencesModel<-glm(graduated~Grade9_Sum_ofUnexcused_absences+Grade10_Sum.of.Unexcused_absence s+Grade11_Sum.of.Unexcused_absences+Grade12_Sum.of.Unexcused_absences+Grade9_Sumoftotal_absences+Grade10_Sum.of.total_absences+Grade11_Sum.of.total_absences +Grade12_Sum.of.total_absences, data=GraduationData, family=binomial()) 
TotalAbsencesModel_Chi<-TotalAbsencesModel$null.deviance-TotalAbsencesModel$deviance
TotalAbsencesModel_DF<-TotalAbsencesModel$df.null-TotalAbsencesModel$df.residual 
TotalAbsencesModel_ChiSquareTest<- 1-pchisq(TotalAbsencesModel_Chi,TotalAbsencesModel_DF) TotalAbsencesModel_ChiSquareTest
exp(TotalAbsencesModel$coefficients)

TardyModel<-glm(graduated~Grade9_SumofDAYS_TARDY+ Grade10_Sum.of.DAYS_TARDY+Grade11_Sum.of. DAYS_TARDY+Grade12_Sum.of.DAYS_TARDY, data=GraduationData, family=binomial()) 
TardyModel_Chi<-TardyModel$null.deviance-TardyModel$deviance 
TardyModel_DF<-TardyModel$df.null-TardyModel$df.residual
TardyModel_ChiSquareTest <- 1-pchisq(TardyModel_Chi,TardyModel_DF)
TardyModel_ChiSquareTest
exp(TardyModel$coefficients)

MembershipModel<-glm(graduated~Grade9_SumofDAYS_IN_MEMBERSHIP+Grade10_Sum.of.DAYS_IN_MEMBERSHIP +Grade11_Sum.of.DAYS_IN_MEMBERSHIP+Grade12_Sum.of.DAYS_IN_MEMBERSHIP, data=GraduationData, family=binomial()) 
MembershipModel_Chi< -MembershipModel$null.deviance-MembershipModel$deviance 
MembershipModel_DF<-MembershipModel$df.null-MembershipModel$df.residual 
MembershipModel_ChiSquareTest<- 1-pchisq(MembershipModel_Chi,MembershipModel_DF)
MembershipModel_ChiSquareTest 
exp(MembershipModel$coefficients)


DropoutModel<-glm(graduated~Grade9_CountofDropout_Flag+Grade10_Count.of.Dropout_Flag+Grade11_Count.of.Dropout_Flag+Grade12_Count.of.Dropout_Flag, data=GraduationData, family=binomial())
DropoutModel_Chi<-DropoutModel$null.deviance-DropoutModel$deviance 
DropoutModel_DF<-DropoutModel $df.null-DropoutModel$df.residual
DropoutModel_ChiSquareTest<- 1-pchisq(DropoutModel_Chi,DropoutModel_DF) 
DropoutModel_ChiSquareTest
exp(DropoutModel$coefficients)

library(popbio)
install.packages (popbio)
logi.hist.plot(GraduationData$Grade9_AverageMarks, GraduationData$graduated, boxp=TRUE, type="hist", col="gray")
cor(GraduationData$Grade9_AverageMarks, GraduationData$Grade9_Sumoftotal_absences, use="complete.obs", method = "pearson")
TotalMarks<-subset(GraduationData, select = c("Grade9_AverageMarks", "Grade10_Average.Marks", "Grade11_Average.Marks", "Grade12_Average.Marks"))
TotalAbsences<-subset(GraduationData, select=c("Grade9_Sumoftotal_absences", "Grade10_Sum.of.total_absences", "Grade11_Sum.of.total_absences", "Grade12_Sum.of.total_absences"))
cor(TotalMarks,TotalAbsences, use="complete.obs", method = "pearson")
plot(TotalMarks,TotalAbsences)


#Principal Component Analysis
library(psych) 
setwd(" ")
FinalData<-read.csv("FinalData.csv") 
FinalData.cor<-cor(FinlData ) cortest.bartlett(FinalData)
FinalData.PCA <-principal(FinalData, nfactors = 26)
print.psych(FinalData.PCA, cut = 0.3, sort = TRUE) 
FinalData.PCA$values
plot(FinalData.PCA$values, type = "b",xlab="factors",ylab="eigenvalues") 
FinalData.PCA2<- principal (FinalData.cor, nfactors = 9)
FinalData.PCA2
print.psych(FinalData.PCA2,cut=0.3,sort=TRUE)


#Classification Analysis

TrainingData<- read.csv("TrainingCategoricalData.csv")
TestData<-read.csv("TestCategoricalData.csv")
library(tree)
FinalCategoricalData<-read.csv("FinalCategoricalData.csv") 
Training<-sample(1:nrow(FinalCategoricalData), .8*nrow(FinalCategoricalData))
TrainingData<-FinalCategoricalData[Training,] 
TestData<-FinalCategoricalData[-Training,]
DecisionTree<-tree(GRADUATED~Highest.Grade+Dropout_Flag+Avg.Marks+Sum.of.DAYS_EXCUSED+Sum.of.TOTAL_ABSENCES+Sum.of.DAYS_ISS+Sum.of.DAYS_TARDY+Sum.of.DAYS_IN_MEMBERSHIP,data=TrainingData)
plot(DecisionTree )
text(DecisionTree, pretty = 0)
TrainingPrediction <-predict(DecisionTree) 
input.graduated<-as.character(TrainingData $GRADUATED) 
pred.graduated<- colnames(TrainingPrediction)[max.col(TrainingPrediction,ties.method=c("first") )]
mean(input.graduated!=pred.graduated) 
DecisionTreePruned<-prune.misclass(DecisionTree, best=4)
plot(DecisionTreePruned)

text(DecisionTreePruned, pretty=0)
fit_tree=tree(TrainingData$GRADUATED~Highest.Grade+Dropout_Flag+Avg.Marks+Sum. of.DAYS_EXCUSED+Sum.of.TOTAL_ABSENCES+Sum.of.DAYS_ISS+Sum.of.DAYS_TARDY+Sum.of.DAYS_IN_MEMBERSHIP,data=TrainingData) 
plot(fit_tree)
text(fit_tree, pretty=0)
test_predictions=predict(fit_tree, TestData, type="class")
test_error<-sum(test_predictions!=TestData$GRADUATED)
test_error/nrow(TestData)


#Cluster Analysis

library(cluster) 
clusplot(csr,km$cluster)
dataCluster <- read.csv(file="dataClusteringColleges.csv")
dataCluster <- zoo::na.locf(dataCluster,na.rm=TRUE)
distance <- dist(dataCluster,method="euclidean")
xy <- data.frame(cmdscale(distance),factor(cluster)) 
hc <- hclust(distance)
cluster <- cuttree(hc,k=3)
xy$factor.cluster. <- factor(xy$factor.cluster.,levels = c(1,2,3), labels=c("Best","Moderate","Low"))
names(xy) <- c("Colleges", "Performance", "clusters")
ggplot(xy, aes(Colleges, Performance)) + geom_point(aes(colour=clusters), size=3)
library("ggplot2", lib.loc="C:/DSBA/R/R-3.2.3/library")
ggplot(xy, aes(Colleges, Performance)) + geom_point(aes(colour=clusters), size=3)
Cluster<- dataCluster [,2:14] 
fmm<-Mclust(Cluster)
library("mclust", lib.loc="C:/DSBA/R/R-3.2.3/library") 
fmm<-Mclust(Cluster)
table(fmm$classification ) kcl<-kmeans(ratings, 3, nstart=25)
kcl<-kmeans(Cluster, 3, nstart=25) 
table(fmm$classification, kcl$cluster) 

install.packages("FactoMineR")
pca<-PCA(Cluster)

help(pca)

fit <- kmeans(Cluster,3)
plot(Cluster,col=fit$cluster,pch=15) 
points(fit$centers,col=1:8,pch=3)

clusplot(Cluster, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
plotcluster(mydata,fit$cluster )

library(fpc) 
plotcluster(mydata,fit$cluster)
plotcluster(Cluster,fit$cluster)
View(Cluster)

Cluster<- Cluster[,1:12]
plotcluster(Cluster,fit$cluster) 
fit <- kmeans(Cluster,3)

clusplot (Cluster, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
distance <- dist( Cluster,method="euclidean" )

hc = hclust(distance) 
plot(hc)







