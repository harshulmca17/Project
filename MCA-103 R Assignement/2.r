//q1 median
k<-fread("loan.csv",skip=1,sep=",")
k<-head(k,-4)
p<-fread("2015.csv",skip=1,sep=",")
p<-head(p,-4)

k<-data.frame(k$loan_amnt)
names(k) <- c("loan_amnt")
p<-data.frame(p$loan_amnt)
names(p)<-c("loan_amnt")

b<-rbind(k,p)

m<-median(b$loan_amnt,na.rm=TRUE)



//q2
k<-fread("loan.csv",skip=1,sep=",")
k<-head(k,-4)
p<-fread("2015.csv",skip=1,sep=",")
p<-head(p,-4)

k<-data.frame(k$purpose)
names(k) <- c("purpose")
p<-data.frame(p$purpose)
names(p)<-c("purpose")

b<-rbind(k,p)
b<-head(b,-4)
c<-as.data.frame(table(b$purpose))


l<-sum(c$Freq)
l

common<-max(c$Freq)
common
//debt_consolidation is the most common purpose
f<-common/l
f
library(MASS)
frac<-fractions(f)
frac

///q3
data14<-fread("loan.csv",skip=1,sep=",")
data15<-fread("2015.csv",skip=1,sep=",")

data14$nrate <- as.numeric(as.character(gsub("%", "", paste(data14$int_rate))))
data15$nrate <- as.numeric(as.character(gsub("%", "", paste(data15$int_rate))))

int_avg14 <- data.frame(data14$purpose, data14$nrate)
names(int_avg14) <- c("Purpose", "trate")
int_avg15 <- data.frame(data15$purpose, data15$nrate)
names(int_avg15) <- c("Purpose", "trate")
int_avg <- rbind(int_avg14, int_avg15)
int_avg$Avg_Rate <- as.numeric(as.character(int_avg$trate))

Rate <- aggregate(Avg_Rate ~ Purpose, int_avg, mean)
ratio <- min(Rate$Avg_Rate)/max(Rate$Avg_Rate)
sprintf("Ratio of minimum average rate to the maximum average rate: %.10f", ratio)


////q4
h<-fread("loan.csv",skip=1,sep=",")[,'term']
h<-head(h,-4)
c<-count(h)
f<-c[1,2]
g<-sum(c$freq)
l<-f/g



p<-fread("2015.csv",skip=1,sep=",")[,'term']
p<-head(p,-4)
c1<-count(p)
c1
f1<-c1[1,2]
g1<-sum(c1$freq)
l1<-f1/g1

diff=l-l1
diff
frac<-fractions(diff)
frac