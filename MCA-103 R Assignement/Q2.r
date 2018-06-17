################################################################################################################################################
 ############################################################### LENDINGCLUB ##################################################################
 ################################################ ANALYSIS OF DATA OF THE YEARS 2014-2015 #####################################################
################################################################################################################################################

#SETTING THE WORKING DIRECTORY TO THE LOCATION OF THE FILE USING "SETWD()"

setwd("/home/harshul/Desktop/MCA-103 Assignmenr")



#CHECKING WHETHER THE WORKING DIRECTORY IS SELECTED ACCORDINGLY USING "GETWD()"

getwd()

#READING THE DATA FILES OF THE YEARS 2014 AND 2015 RESPECTIVELY USING "READ.CSV()"

#LOANSTATS3C.CSV ---> YEAR 2014 DATA

#STORING THE 2014 DATA IN TO THE VARIABLE DATA1

data1<-read.csv('LoanStats3c.csv',skip=1,header=T)




#LOANSTATS3D.CSV ---> YEAR 2015 DATA


#STORING THE 2015 DATA IN TO THE VARIABLE DATA2


data2<-read.csv('LoanStats3d.csv',skip=1,header=T)



#COMBINING BOTH THE FILES IN A DIFFERENT VARIABLE DATA3 USING "RBIND()"


data3<-rbind(data1,data2)



#CHECKING THE COMBINED DATA BY CALLING THE VARIABLE DATA3

data3



#                                          QUESTION 1 : WHAT IS THE MEDIAN LOAN AMOUNT ?


#FETCHING THE LOAN_AMNT COLUMN FROM THE COMBINED DATA USING "$" AND STORING IT INTO "LOAN_AMOUNT" VARIABLE

Loan_Amount<-data3$loan_amnt


#READING THE LOAN AMOUNT DATA

#BOUND REACHED, INCREASING THE BOUNDARY LEVEL USING "OPTIONS()"


options('max.print'=99999999)

#READING LOAN AMOUNT DATA AGAIN

Loan_Amount

#ARRANGING DATA IN A COLUMN MAJOR FORM USING CBIND()

cbind(Loan_Amount)


#REMOVING THE NA VALUES FROM LOAN_AMOUNT AND STORING RESULT INTO "CORRECTED_LOAN_AMOUNT"

Corrected_Loan_Amount = na.omit(Loan_Amount)


#READING CORRECTED LOAN AMOUNT

Corrected_Loan_Amount


#CALCULATING MEDIAN AND STORING IT INTO MEDIANLOANAMOUNT

MedianLoanAmount<-median(Corrected_Loan_Amount)



#READING THE MEDIAN OF THE LOAN AMOUNT (FINAL SOLUTION FOR QUESTION 1.)

MedianLoanAmount


#         QUESTION 2: EACH LOAN IS CATEGORIZED INTO A SINGLE PURPOSE. WHAT FRACTION OF ALL LOANS ARE FOR THE MOST COMMON PURPOSE?
#FETCHING PURPOSE FROM THE DATA AND STORING IT INTO "PURPOSE"


purpose<-data3$purpose



#READING THE PURPOSE COLUMN

purpose

cbind(purpose)

#FINDING THE OCCURENCES OF DIFFERENT PURPOSES USING "TABLE(UNLIST(X))" AND STORING IT IN "OCCURENCES"


Occurences<-table(unlist(purpose))


#READING THE OCCURENCES OF DIFFERENT PURPOSES

Occurences


#OBSERVATION :- DEBT_CONSOLIDATION IS THE MOST COMMON PURPOSE AMONG ALL THE PURPOSES 

#RETRIEVING THE OCCURENCE OF DEBT_CONSOLIDATION AND STORING IT IN "MOST_COMMON"


most_common<-Occurences["debt_consolidation"]

most_common


#CALCULATING THE TOTAL NO. OF ENTRIES IN PURPOSE FIELD AND STORING IT IN "LENGTH_PURPOSE"

Length_Purpose<-length(purpose)

Length_Purpose


#CALCULATING THE FRACTION AND STORING IT INTO THE "FRACTION"

fraction<-most_common/Length_Purpose



#READING THE FRACTION VALUE ( FINAL ANSWER TO QUESTION 2.)

fraction


#WE GOT THE FRACTION UPTO 7 DECIMAL PLACE , WE WILL EXTEND IT UPTO 10 DECIMAL PLACES USING "OPTIONS(DIGITS="")"

options(digits=10)

fraction




#QUESTION3: CALCULATE THE AVERAGE INTEREST RATE ACROSS LOANS FOR EACH PURPOSE. WHAT IS THE RATIO OF MINIMUM AVERAGE RATE TO THE MAXIMUM AVERAGE RATE? (THE RATIO SHOULD BE LESS THAN 1.) 

#SELECT THE PURPOSE COLUMN AND STORING IT TO "PURPOSES"

Purposes<-data3$purpose



#SELECT THE INTEREST RATE COLUMN AND STORING IT INTO "RATE"

Rate<-data3$int_rate



#STORING RATE VALUES IN INTEGER FORM USING 

Rate1<-as.numeric(sub("%"," ",Rate))/100

 

#FINDING EACH PURPOSE AVERAGE INTEREST RATE CALCULATION USING "TABLE(UNLIST())"


names(table(unlist(Purposes)))




#FINDING MEANS OF RATES FOR PURPOSE CAR I.E [2]

mean_car<-mean(subset(Rate1,Purposes=="car"))


#FINDING MEAN OF RATES FOR PURPOSE CREDIT_CARD I.E [3]

mean_creditcard<-mean(subset(Rate1,Purposes=="credit_card"))


#FINDING MEAN OF RATES FOR PURPOSE DEBT_CONSOLIDATION I.E [4]

mean_debt<-mean(subset(Rate1,Purposes=="debt_consolidation"))


#FINDING MEAN OF RATES FOR PURPOSE HOME_IMPROVEMENT I.E [5]

mean_home<-mean(subset(Rate1,Purposes=="home_improvement"))


#FINDING MEAN OF RATES FOR PURPOSE HOUSE I.E.[6]

mean_house<-mean(subset(Rate1,Purposes=="house"))


#FINDING MEAN OF RATES FOR PURPOSE MAJOR_PURCHASE I.E.[7]

mean_mp<-mean(subset(Rate1,Purposes=="major_purchase"))


#FINDING MEAN OF RATES FOR PURPOSE MEDICAL I.E.[8]

mean_med<-mean(subset(Rate1,Purposes=="medical"))


#FINDING MEAN OF RATES FOR PURPOSE MOVING I.E.[9]
 
mean_mov<-mean(subset(Rate1,Purposes=="moving"))


#FINDING MEAN OF RATES FOR PURPOSE OTHER I.E.[10]

mean_oth<-mean(subset(Rate1,Purposes=="other"))


#FINDING MEAN OF RATES FOR PURPOSE RENEWABLE_ENERGY I.E.[11]

mean_re<-mean(subset(Rate1,Purposes=="renewable_energy"))


#FINDING MEAN OF RATES FOR PURPOSE SMALL_BUSINESS I.E.[12]

mean_sb<-mean(subset(Rate1,Purposes=="small_business"))


#FINDING MEAN OF RATES FOR PURPOSE VACATION I.E.[13]

mean_vac<-mean(subset(Rate1,Purposes=="vacation"))


#FINDING MEAN OF RATES FOR PURPOSE WEDDING I.E.[14]

mean_wed<-mean(subset(Rate1,Purposes=="wedding",na.rm=T))


#FINDING MEAN OF RATES FOR PURPOSE EDUCATIONAL I.E.[15]

mean_edu<-mean(subset(Rate1,Purposes=="educational"))




#FINDING MAXIMUM OF ALL MEANS USING THE FUNCTION MAX()



maxMean<-max(mean_car,mean_creditcard,mean_debt,mean_home,mean_house,mean_mp,mean_med,mean_mov,mean_oth,mean_re,mean_sb,mean_vac,mean_wed,mean_edu)

minMean<-min(mean_car,mean_creditcard,mean_debt,mean_home,mean_house,mean_mp,mean_med,mean_mov,mean_oth,mean_re,mean_sb,mean_vac,mean_wed,mean_edu)

Ratio<-minMean/maxMean

Ratio



#                QUESTION 4.  WHAT IS THE DIFFERENCE IN THE FRACTION OF THE LOANS WITH A 36-MONTH TERM BETWEEN 2014 AND 2015?  



#FETCHING TERM FROM 2014 DATA AND STORING IT INTO "TERM1"

Term1<-(data1$term)

count<-(unlist(table(Term1)))



#STORING THE COUNT USING THE INDEX OF 36 MONTHS

calc1<-count[2]



#FRACTION CALCULATION FOR 2014 "FRAC2014"

frac2014<-calc1/length(Term1)



#FETCHING TERM FROM 2015 DATA AND STORING IT INTO "TERM2"


Term2<-(data2$term)

count2<-(unlist(table(Term2)))



#STORING THE COUNT USING THE INDEX OF 36 MONTHS

calc2<-count2[2]



#FRACTION CALCULATION FOR 2015 "FRAC2015"

frac2015<-calc2/length(Term2)




#STORING THE FINAL RESULT(DIFFERENCE) INTO "RESULT"


Result<-frac2014-frac2015




#QUESTION5. I WILL CONSIDER ALL LOANS THAT ARE NOT IN THE 'FULLY PAID', 'CURRENT', 'IN GRACE PERIOD' STATUSES TO BE IN DEFAULT. CALCULATE THE RATIO OF THE TIME SPENT PAYING THE LOAN, DEFINED AS THE DIFFERENCE BETWEEN THE LAST PAYMENT DATE AND THE ISSUE DATE, DIVIDED BY THE TERM OF LOAN. WHAT IS THE STANDARD DEVIATION OF THIS RATIO FOR ALL THE LOANS IN DEFAULT? 



#STORING THE LOAN_STATUS TO VAR "STATUS"

Status<-data3$loan_status




#EXTRACTING THE REQUIRED SUBSET


Default<-subset(data3,Status!='Fully Paid' & Status!='Current'& Status!='In Grace Period')




#STORING ISSUED AND LAST PAYMENT DATES TO RESPECTIVE VARIABLES


Issued_date<-subset(data3$issue_d,Status!='Fully Paid' & Status!='Current'& Status!='In Grace Period')

last_payment_date<-subset(last_payment_date,Status!='Fully Paid' & Status!='Current'& Status!='In Grace Period')

I_date<-paste("01-", Issued_date , sep = "")
 
Issued_date1<-as.Date(I_date, format = "%d-%b-%Y")

P_date<-paste("01-", last_payment_date , sep = "")

Payment_Date1<-as.Date(P_date, format = "%d-%b-%Y")



#FINAL CALCULATION(SOLUTION TO QUESTION 5)


Time_Spent<- (Payment_Date1 - Issued_date1)

term<-as.numeric(gsub("months" ,"" ,Default$term))

ratio<-Time_Spent/(term*30)

S.D.<-sd(ratio,na.rm=TRUE)


#QUESTION 6.  WHAT IS THE MEAN, MEDIAN, MEAN ABSOLUTE DEVIATION, VARIANCE, IQR, SKEWNESS AND KURTOSIS FOR THE TOTAL RATE OF RETURN, AS FIGURED FROM THE TOTAL PAYMENTS AND THE LOAN AMOUNT, AND THE INTEREST RATE? CONSIDER ONLY LOANS THAT HAVE REACHED THE END OF THEIR TERM. [SUMMARY FUNCTION NOT TO BE USED HERE]



data3<-cbind(data3,Rate1)

Paid_loan<-subset(data3,loan_status=="Fully Paid")



#CALCULATING RETURN RATE

 
new<-as.numeric(Paid_loan$loan_amnt*Paid_loan$Rate1)+as.numeric(Paid_loan$loan_amnt)

new2<-new - Paid_loan$total_pymnt

Ret_Rate<-new2/Paid_loan$loan_amnt




#CALCULATION OF MEAN,MEDIAN,MEANABSOLUTEDEVIATION,VARIANCE,IQR AND MOMENTS


MeanRR<-mean(Ret_Rate,na.rm=TRUE)

MedianRR<-median(Ret_Rate,na.rm=TRUE)

mean_abs_devRR<-aad(Ret_Rate,na.rm=TRUE)

varianceRR<-var(Ret_Rate,na.rm=TRUE)

skewnessRR<-skewness(Ret_Rate,na.rm=TRUE)

KurtosisRR<-kurtosis(Ret_Rate,na.rm=TRUE)

IQR_RR<- IQR(Ret_Rate,na.rm=TRUE)




