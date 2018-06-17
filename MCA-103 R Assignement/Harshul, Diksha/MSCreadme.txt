################################################################################################################################################
##############################################################  STATISTICS CALCULATOR     ######################################################
################################################################################################################################################ 
##############################################################   MCA-103 Assignment 3     #######################################################
################################################################################################################################################


-----------------------------------------------------------------------------------------------------------------------------------------------
                                                 EXTERNAL  FUNCTIONS USED 

                               FUNCTION NAME                   :               ARGUMENTS
                           UserRemoveNA                        :                input
                           UserSort                            :                input
                           UserSortKruskal                     :                input
                           SortRank                            :                data_minus_mu 
                           UserFactorial                       :                num
                           Intersection                        :                input1,input2
                           gamma                               :                alpha
 
--------------------------------------------------------------------------------------------------------------------------------------------------

                                                        DESCRIPTION OF FUNCTIONS 		

  
  UserRemoveNA() : 
                  Objective:       Removes all NA's that are entered by user in the input
                  InputParameters: input(a vector containing integer values)
                  ReturnValue:     Input after removing NA's
  UserSort():
                 Objective:       Sorts the input vector
                 InputParameters: input(a vector containing integer or numeric values)
                 ReturnValue:     Input after sorting
 
  UserSortKruskal():
                    Objective:       Sorts the input vector and also sorts the name of input after sorting
                    InputParameters: input(a vector containg integer or numeric values)
                    ReturnValue:     input after Sorting
  SortRank():
                   Objective:       Sort an array according to its magnitude and provide them with their respective rank
                   InputParameters: data_minus_mu (array of difference between data and mu )
                   ReturnValue:     sorted array with their respective ranks   
   UserFactorial():
                   Objective:       Calculates the factorial of given number
                   InputParameters: num
                   ReturnValue:     Factorial of inputted num(number)
            
            
   Intersection():
                   Objective:       Calculates the probability of intersection of two given inputs
                   InputParameters: input1,input2(two vectors whose intersection is being calculated)
                   ReturnValue:     Probability of intersection of two vectors i.e. P(AB)
 
         gamma():
                   Objective:       Calculates gamma values of given alpha
                   InputParameters: alpha
                   ReturnValue:     Gamma Value
            
################################################################################################################################################

                   

                                                                MODULE1
                                                        DESCRIPTIVE ANALYSIS

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                   FUNCTIONS USED 

                               FUNCTION NAME                   :               ARGUMENTS
                                 UserMean                       :                data  
                                 UserMedian                    :                 data
                                 UserMode                      :                 data
                                 UserMeanAD                    :                 data
                                 UserVariance                  :                 data
                                 UserSD                        :                 data
                                 UserRange                     :                 data
                                 UserQuartile                  :                 data
                                 UserIQR                       :                 data
                                 UserMaximum                   :                 data
                                 UserMinimum                   :                 data
                                 UserMoments                   :                 data
                                 UserSkewness                  :                 data
                                 UserKurtosis                  :                 data
                                 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                     DESCRIPTION OF FUNCTIONS 		

     
        UserMean():
                   Objective:       Calculates mean of data after removing NA's from input(by taking sum of elements of data and dividing it by length of data)
                   InputParameters: data(a vector containg integer or numeric values)
                   ReturnValue:     Mean of the data
            
     UserMedian():           
                  Objective:       Calculates median of data after removing NA's from input(by dividing the data into half)
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Median of data

      UserMode():
                  Objective:       Calculates the mode(highest frequency) of data(by taking count of each element and returning the maximum count)
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Value in dataset having highest frequency 
     UserMeanAD():
                  Objective:       This function measures the dispersion of given data by calculating the simple arithematic mean of the deviation
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Mean Absolute Deviation of data given
     
   UserVariance():
                  Objective:       This function measures the spread of data by taking the average squared distance of the observations from the sample mean. 
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Sample variance of given data

         UserSD():
                  Objective:       Measures the dispersion by taking square root of variance
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Sample Standard Deviation
      UserRange():
                  Objective:       Measures the dispersion of data by difference between the highest and lowest given values
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Range of data

     UserQuartile():
                   Objective:      Calculates quartile of data i.e. the points through which data is divided into four parts
                   InputParameters: data(a vector containg integer or numeric values)
                   ReturnValue:     Q1(upper quartile),Q2(median),Q3(lower quartile)
 
       UserIQR():
                  Objective:       Measures the spread of data by taking difference of upper and lower quartiles and dividing it by 2
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Quartile Deviation of data

    UserMaximum():
                  Objective:       Finds the maximum value in data
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     maximum value

    UserMinimum():
                  Objective:       Finds the minimum value in dataset 
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     minimum value

    UserMoments():            
                  Objective:       Calculates the rth moment about arithmetic mean i.e. the CENTRAL MOMENT
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     First four value of Central Moment

   UserSkewness():            

                  Objective:       Measures the shape of given data(the sign of g1 indicates the direction of skewness of the distribution. Samples that have g1 > 0 indicate right-skewed distributions
                                   (or positively skewed), and samples with g1 < 0 indicate left-skewed distributions (or negatively skewed). Values of g1 near zero indicate a symmetric distribution.
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Value of Skewness

   UserKurtosis():
                  Objective:       Measures the peakedness of dataset(It takes values -2 = g2 < 8. The subtraction of 3 is done so that mound shaped samples have values of g2 near zero. Samples with g2 > 0 are
                                   called "leptokurtic",samples with g2 < 0 are called "platykurtic", g2 = 0 are called "mesokurtic")
                  InputParameters: data(a vector containg integer or numeric values)
                  ReturnValue:     Value of Kurtosis i.e. g2

 
################################################################################################################################################


                                                                    MODULE 2
                                                                 PREDICTIVE ANALYSIS

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  FUNCTIONS USED 

                               FUNCTION NAME                   :               ARGUMENTS
                                 UserCorrelation               :                 input,input1 
                   UserMultipleLinearRegression                :                 y,x1,x2
                                 
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

                                                       DESCRIPTION OF FUNCTIONS 		



UserCorrelation():     
                  Objective:       Finds the strength of relationship between the two data(if the value is near to mod(data) then it is a "strong relationship,if it is close to zero then 
                                   is a "weak relationship")
                  InputParameters: input,input1
                  ReturnValue:     Value of correlation i.e. r

UserMultipleLinearRegression():
                 Objective:       Predicts a data with the help of two predictors given 
                 InputParameters: y(whose value is predicted),x1 and x2(the two predictors i.e. independent data)
                 ReturnValue:     An equation representing the relationship between predictors and predicted data


#######################################################################################################################################################################

                                           MODULE 3
                                      PROBABILITY ANALYSIS

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                      FUNCTION NAME             :               ARGUMENTS
                           UserPermutation                      :                num1,num2
                           UserCombination                      :                num1,num2
			   UserBprobability                     :                sample,event
                           UserConditionalP                     :                sample,A,B
                            UserBayesT                          :                PRIOR,LIKE
                        
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                            DESCRIPTION OF FUNCTIONS 		


UserPermutation():
                   Objective:      Finds the number of permutations of n objects taken r at a time
                   InputParameters:num1(n values),num2(r values taken at a time from n)
                   ReturnValue:    total number of arrangements i.e. P(n,r)

UserCombination(): 
                   Objective:       Finds value of selection of items from a dataset , such that the order of selection does not matter. 
                   InputParameters: num1,num2
                   ReturnValue:     number of ways to select r values from n i.e. C(n,r)

UserBprobability(): 
                   Objective:       Finds the probability of an event (by dividing favourable outcomes by total number of outcomes)
                   InputParameters:sample(set of possible outcomes),event(set of favourable outcome from given sample)
                   ReturnValue:     Probability of event
UserConditionalP():
                   Objective:       Finds the probability of A(given B) and probability of B(given A)(by finding the intersection of the two events)
                   InputParameters: sample,A and B(two events that are subsets of given sample)
                   ReturnValue:      Value of P(A|B) and P(B|A)
     UserBayesT():
                   Objective:       Finds the probability of an event, based on prior knowledge of conditions that might be related to the event
                   InputParameters: prior(vector of prior probability of possible events),like(vector of likelihood )
                   ReturnValue:     Value of probability


################################################################################################################################################

                                                                    MODULE 4
                                                            DISCRETE DISTRIBUTION FUNCTIONS

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                      FUNCTION NAME             :               ARGUMENTS
                           UserUniformDist                      :                  x
                           UserBernoulli                        :                 theta,x
			   UserGeometric                        :                theta,x
                           UserHypergeometric                   :               N,n,M,m
                           UserNegativeBinom                    :               theta,k,x
                           UserPoisson                          :               theta,n,x
                           UserMultinomial                      :               n,x,pis
                           UserMultivarHyperGeo                 :               n,x,Mis
                        
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                            DESCRIPTION OF FUNCTIONS 		



  UserUniformDist():
                    Objective:       Calculates the probability distribution function of any random variable x
                    InputParameters: x(a vector)
                    ReturnValue:    pdf of random variable any value in vector x

     UserBernoulli():
                    Objective:       Calculates the probability distribution function of bernoulli random variable x(0 or 1)
                    InputParameters: theta(probability of success),x(number of successes(either 0 or 1))
                    ReturnValue:     value of pdf 

     UserBinomial():
                    Objective:        Calculates the probability distribution function of binomial random variable x(repeated trials)
                    InputParameters:  theta(probability of success),n(number of trials),x(number of successes)
                    ReturnValue:      value of pdf 

   UserGeometric():
                   Objective:        Calculates the probability distribution function of geometric random variable x(Special case of Negative Binomial when k=1)
                   InputParameters:  theta(probability of success),x(success on xth trial)
                   ReturnValue:      value of pdf

UserHypergeometric():
                   Objective:       Calculates the probability distribution function of Hypergeometric random variable x
                   InputParameters: N(total number of elements),n(number of trials from N elements),M(success from N elements),x(Number of successes in n trial)
                   ReturnValue:     value of pdf

UserNegativeBinom():
                   Objective:       Calculates the probability distribution function of negative binomial random variable x
                   InputParameters: theta(probability of success),k(number of trial),x(number of success-1)
                   ReturnValue:     value of pdf

    UserPoisson():
                   Objective:       Calculates the probability distribution function of poisson random variable x(Special case of binomial when n is very large and theta is small)
                   InputParameters: theta(probability of success),n,x
                   ReturnValue:     value of pdf

 UserMultinomial():
                   Objective:       Calculates the probability distribution function of multinomial random vector x
                   InputParameters: n(total values in vector x),x(containing random variables),pis(probability of success each element in xis)
                   ReturnValue:     value of pdf 

UserMultivarHyperGeo():             
                   Objective:       Calculates the probability distribution function of multivariate hypergeometric random vector x
                   InputParameters: n(number of trials),xis(vector containing value of possible outcomes),Mis(Elements of kinds of possin=ble outcomes)
                   ReturnValue:     value of pdf


#########################################################################################################################################################
                                                                            MODULE 5
                                                                    CONTINOUS DISTRIBUTION FUNCTION

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                      FUNCTION NAME             :               ARGUMENTS
                           UserUniformContinous                 :               lower,upper,alpha,beta
                           UserNormalDist			:               lower,upper,mu,sigma
			   UserGammaDist			:               lower,upper,alpha,beta
                           UserExpoDist 			:               lower,upper,theta
                           UserBivariateNormal 			:               xi,yi
                           
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                            DESCRIPTION OF FUNCTIONS 	
   
UserUniformContinous():  
                   Objective:        Calculates the probability density function of uniform continous random variable x(taking value from lower to upper)
                   InputParameters:  lower and upper(a random variable),alpha(a constant),beta(a constant(should be greater then alpha))
                   ReturnValue:      pdf of given limit

  UserNormalDist():
                   Objective:       Calculates the probability density function of normal distribution random variable x(taking value from lower to upper)
                   InputParameters: lower and upper(a random variable),mu(mean),sigma(standard deviation)
                   ReturnValue:     pdf of given limit

   UserGammaDist():
                   Objective:       Calculates the probability density function of gamma distribution random variable x(taking value from lower to upper)
                   InputParameters: lower and upper(a random variable),alpha,beta
                   ReturnValue:     pdf of given limit

    UserExpoDist():
                   Objective:       Calculates the probability density function of exponential distribution random variable x(taking value from lower to upper)
                   InputParameters: theta,lower and upper(a random variable)
                   ReturnValue:     pdf of given limit
 
UserBivariateNormal():              
                   Objective:       Calculates the probability density function of Bivariate Normal distribution random variable x(taking value from lower to upper)
                   InputParameters: xi(vector of data1),yi(vector of data2)
                   ReturnValue:     pdf of given limit



#########################################################################################################################################################
                                                                            MODULE 6
                                                                   SAMPLE DISTRIBUTION TEST STATISTICS

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                      FUNCTION NAME             :               ARGUMENTS
                           UserChiSqTest			:               data,popVar,alpha,case
                           UserStudentTtest			:               data,mu,alpha,case
			   UserFDistTest			:               data1,data2,alpha,case
                           UserZTest				:               sigma,mu,n,alpha,xbar,case
                           
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                         DESCRIPTION OF FUNCTIONS 	
   

  UserChiSqTest():  
                   Objective:       Calculates Chi Squared Distribution with n-1 degree of freedom and test the hypothesis(by comparing the calculated value with the 
                                    critical value from chisq table)
                   InputParameters: data(sample data),popVar(variance of population),alpha(level of significance),case(case=1(two tail test),case=2(right tail),case=3(left tail))
                   ReturnValue:     Result of hypothesis testing

  UserStudentTtest():
                   Objective:       Calculates Chi Student T Distribution with n-1 degree of freedom and test the hypothesis(by comparing the calculated value with the 
                                    critical value from t table)
                   InputParameters: data(sample data),mu(mean of population),alpha(level of significance),case(case=1(two tail test),case=2(right tail),case=3(left tail))
                   ReturnValue:      Result of hypothesis testing

   UserFDistTest():
                   Objective:       Calculates Chi F Distribution with (n-1,m-1) degree of freedom and test the hypothesis(by comparing the calculated value with the 
                                    critical value from f table)
                   InputParameters: data1,data2(two sample datas),alpha(level of significance),case(case=1(two tail test),case=2(right tail),case=3(left tail))
                   ReturnValue:      Result of hypothesis testing

    UserZTest():
                   Objective:        Calculates Chi Z Test Distribution and tests the hypothesis(by comparing the calculated value with the 
                                     critical value from z table)
                   InputParameters:  sigma(standard deviation of population),mu(mean of population),n,alpha(level of significance),xbar,case(case=1(two tail test),case=2(right tail),case=3(left tail))
                   ReturnValue:      Result of hypothesis testing
 



#########################################################################################################################################################
                                                                            MODULE 7
                                                                       INTERVAL ESTIMATION
   --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                      FUNCTION NAME             :               ARGUMENTS
                            UserMeanEst				:               xi,alpha,psigma,mu
                           UserDiffInMeanEst			:               x1,x2,alpha,variance1,variance2
			   UserProportionEst			:               percentCI,x,n
                           UserDiffInPropor			:               x1,n1,x2,n2,percentCI
                           UserEstVariance			:               x,percentCi
                           UserEstRatioVariance			:               data1,data2,percentCI

                           
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                 DESCRIPTION OF FUNCTIONS 	
   

     UserMeanEst():  
                   Objective:       Calculates the value of confidence interval using estimation of mean
                   InputParameters: xi(data on which interval is calculated),alpha(level of significance),psigma(standard deviation of population in case of unknown),mu(mean of population)
                   ReturnValue:     Confidence limits of given data

   UserDiffInMeanEst():
                   Objective:       Calculates the value of confidence interval using difference in mean
                   InputParameters: x1,x2,(two vectors on which interval is calculated),alpha(level of significance),variance1,variance2(variances of two populations),mu1, mu2 
                   ReturnValue:     Confidence limits of given data

   UserProportionEst():
                   Objective:       Calculates the value of confidence interval using proportion estimation
                   InputParameters: percentCI(percentage of confidence interval),x(length of an event from sample),n(length of sample)
                   ReturnValue:      Confidence limits of given data
 
 UserDiffInPropor():
                   Objective:       Calculates the value of confidence interval using difference in proportion estimation
                   InputParameters: x1(length of an event from sample1),n1(length of sample1),x2(length of an event from sample2),n2(length of sample1),percentCI(percentage of confidence interval)
                   ReturnValue:      Confidence limits of given data

UserEstVariance():
                   Objective:       Calculates the value of confidence interval using estimation of Variance
                   InputParameters: x(a vector on which interval is calculated),percentCI(percentage of confidence interval)
                   ReturnValue:      Confidence limits of given data


UserEstRatioVariance(): 
                   Objective:       Calculates the value of confidence interval using estimation of ratio variance
                   InputParameters: data1,data2(sample vectors),percentCI(percentage of confidence interval)
                   ReturnValue:      Confidence limits of given data


#########################################################################################################################################################
                                                                            MODULE 8
                                                                 NON-PARAMETRIC ANALYSIS

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                      FUNCTION NAME             :               ARGUMENTS
                           UserSignT			        :               data,mu,alpha
                           UserWilcoxonTest			:               data,mu,alpha,case
			   UserMannWTest			:               data1,data2,alpha,case
                           UserKruskalWTest		        :               data1,data2,data3,alpha
                           
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                      DESCRIPTION OF FUNCTIONS 	
   
 
       UserSignT():  
                   Objective:       Calculates value of sign test(alternative to one sample t test)and accordingly accepts or rejects Ho    
                   InputParameters: data(vector on which the analysis is performed),mu(value to be used to check the null hypotheis),alpha(level of significance)
                   ReturnValue:     Accepts or rejects the null hypothesis

 UserWilcoxonTest():
                   Objective:      Calculates value of wilcoxon test and accordingly accepts or rejects Ho 
                   InputParameters: data(vector on which the analysis is performed),mu(value to be used to check the null hypotheis),alpha(level of significance),case(case=1(two tail test),case=2(right tail),case=3(left tail))
                   ReturnValue:      Accepts or rejects the null hypothesis


    UserMannWTest():
                   Objective:       Calculates value of Mann Whitney test and accordingly accepts or rejects Ho
                   InputParameters: data1,data2(vectors on which the analysis is performed),alpha(level of significance),case(case=1(two tail test),case=2(right tail),case=3(left tail))
                   ReturnValue:      Accepts or rejects the null hypothesis


UserKruskalWTest():
                   Objective:       Calculates value of Kuruskals wallis test and accordingly accepts or rejects Ho
                   InputParameters: data1,data2,data3(vectors on which the analysis is performed),alpha(level of significance)
                   ReturnValue:     Accepts or rejects the null hypothesis

 


                   
#########################################################################################################################################################
                                                                      MODULE 9
                                                                    VISUALIZATION 
                  
   --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                      FUNCTION NAME             :               ARGUMENTS
                           UserHist                             :                  data
                           UserLineGraph                        :                  data
			   UserBarPlot                          :                  data
                           UserPie                              :                  data
                           UserScatter                          :                  data
                           UserBoxPlot                          :                  data
                           UserQPlot                            :                  data
                           UserStem                             :                  data
                        
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                            DESCRIPTION OF FUNCTIONS 		
        
    UserHist(): 
                   Objective:       Draw histogram of given data
                   InputParameters: data
                   ReturnValue:     Graphical representation of data

    UserLineGraph():
                   Objective:       Draw Line Graph of given data
                   InputParameters: data
                   ReturnValue:     Graphical representation of data

    UserBarPlot():
                   Objective:       Draw Bar PLot of given data
                   InputParameters: data
                   ReturnValue:     Graphical representation of data

    UserPie():
                   Objective:       Draw Pie Chart of given data
                   InputParameters: data
                   ReturnValue:     Graphical representation of data

    UserScatter():
                   Objective:       Draw Scatter Graph  of given data
                   InputParameters: data, data1
                   ReturnValue:     Graphical representation of data
    UserBoxPlot():
                   Objective:       Draw Box Plot of given data
                   InputParameters: data, data1
                   ReturnValue:     Graphical representation of data
    UserQplot():
                   Objective:       Draw Q Plot of given data
                   InputParameters: data, data1
                   ReturnValue:     Graphical representation of data
    UserStem():
                   Objective:       Draw Stem of given data
                   InputParameters: data
                   ReturnValue:     Graphical representation of data

--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  In-built functions were used only in Visualisations and Functions with Test Statististics ( Z-test ,T-test , Chi-Square-test,F-test,Sign rank test, U test,H-test)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------

                                 
                                               GROUP MEMBERS
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
				NAME :		  Roll No.:	Class:

				Diksha Verma	   11	 	MCA 1st year
               			Harshul Kumar 	   14	 	MCA 1st year
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
