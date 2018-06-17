
UserRemoveNA <- function(input){
  input1<-is.na(input)
  input1<-as.numeric(input1)
  input2<-input[!input1]
  return(input2)
}

UserSort<-function(input){
  temp=0
  for( i in 1:length(input))
  {
    for( j in i:length(input))
    {
      if(input[i] > input[j])
      {
        temp = input[i]
        input[i] = input[j]
        input[j] = temp
      }
    }
  }
  return(input)
}

UserSortKruskal <- function(input){
  temp=0
  for( i in 1:length(input))
  {
    for( j in i:length(input))
    {
      if(input[i] > input[j])
      {
        temp = input[i]
        input[i] = input[j]
        input[j] = temp
        
        temp <- names(input[i]) 
        names(input)[i]<-paste(names(input[j]))
        names(input)[j]<-paste(temp)
      }
    }
  }
  return(input)
}

SortRank <- function(data_minus_mu){
  
  temp=0
  a<- 0
  b<- 0  
  data_minus_mu_abs = abs(data_minus_mu) 
  for( i in 1:length(data_minus_mu))
  {
    for( j in i:length(data_minus_mu))
    {
      if(data_minus_mu_abs[i] > data_minus_mu_abs[j])
      {
        temp = data_minus_mu_abs[i]
        data_minus_mu_abs[i] = data_minus_mu_abs[j]
        data_minus_mu_abs[j] = temp
        
        temp = data_minus_mu[i]
        data_minus_mu[i] = data_minus_mu[j]
        data_minus_mu[j] = temp
      }
    }
  }
  return(data_minus_mu)
}

################################################# Module 1 #############################################################################

UserSD <- function(data){
  sum = 0.0 
  mean = 0
  standardDeviation = 0.0
  
  for(i in 1:length(data))
  {
    sum <- sum + data[i]
  }
  
  mean = sum/length(data)
  
  for(i in 1:length(data))
    standardDeviation = standardDeviation + ((data[i] - mean)^2)
  
  return(sqrt(standardDeviation / (length(data)-1)))
}

UserVariance <-function(input)
{sum1<-0
dinp<-as.numeric()
n<-length(input)
a<-mean(input)
for(i in 1:n)
{ y<-(input[i]-a)
dinp<-c(dinp,y)
}
sqinp<-dinp^2
for(i in 1:n)
{ sum1<-sum1+sqinp[i]
}
var<-sum1/(n-1)
var
}

UserMean <- function(data){
  sum = 0.0 
  mean = 0

  for(i in 1:length(data))
  {
    sum <- sum + data[i]
  }
  
  mean = sum/length(data)
  return(mean)
  
}

UserMedian <- function(data){
  index = 0
  median = 0
  size = length(data)
  if (size %% 2 == 0)
  {
    
    median = (data[length(data)/2] + data[(length(data)/2 + 1)]) / 2.0;
    
  }
  else
  {
    median = data[(length(data)+1)/2];
    
  
    }
  return(median);
}

UserMode <- function(data){
  
  mode =as.numeric()
  count = 1
  
  count_Ele = table(data)
  max_count = UserMaximum(count_Ele)
  for(i in 1:length(count_Ele))
  {
    if( max_count == count_Ele[i])
    {
      mode = c(mode,names(count_Ele[i]))
    }
      
  }
  
  return(as.numeric(mode))
}

UserMeanAD  <-function(input){
  sum1<-0
  n<-length(input)
  a<-mean(input)
  dinp <- c(0)
  for(i in 1:n)
  {
    y<-(input[i]-a)
    dinp<-c(dinp,y)
  }
  dinp<-abs(dinp)
  for(i in 1:n)
  { sum1<-sum1+dinp[i]
  }
  MAD<-sum1/(n-1)
  
  return(MAD)
}

UserQuartile <- function(input)
{
  input<- UserSort(input)
  n <- length(input)
  if(n%%2==0)
  {
    l<-n/2
    u<-(n/2)+1
  }
  else
  {
    n1<-(n+1)/2
    l<-n1
    u<-n1+1
  }
  input1=input[1:l]
  input2=input[u:n]
  x<-c(Q1=median(input1),Q2=median(input), Q3=median(input2))
  return(x)
}  

UserRange <- function(data){
  
  return(UserMaximum(data)-UserMinimum(data))
  
}

UserIQR <-function(input)
{
  quartile<-Quartile(input)
  q3<-quartile[3]
  q1<-quartile[1]
  iqr<-(q3-q1)/2
  return(iqr)
}

UserMaximum <- function(data){
  
  max = data[1]
  
  for (i in 1:length(data))
  {
    
    if( data[i] > max)
    {
      max = data[i]
    }
  }
  return(max)
}

UserMinimum <- function(data){
  
  min = data[1]
  
  for (i in 1:length(data))
  {
    
    if( data[i] < min)
    {
      min = data[i]
    }
  }
  return(min)
}

UserMoments <-function(data){
    dataInput<-as.numeric()
    datamean <-UserMean(data)
    size<-length(data)
    for(i in 1:size)
    {
        d<-(data[i]-datamean)
        dataInput<-c(dataInput,d)
    }
    mu1 <-mean(dataInput)
    dataInput1<-dataInput^2
    mu2 <-mean(dataInput1)
    
    dataInput2<-dataInput^3
    mu3 <-mean(dataInput2)
    
    dataInput3<-dataInput^4
    mu4 <-mean(dataInput3)
    
    moment<-c(mu1,mu2,mu3,mu4)
    names(moment)<-c("mu1","mu2","mu3","mu4")
    return(moment)
}

UserSkewness<-function(data){
  dataInput<-as.numeric()
  datamean<-UserMean(data)
  size<-length(data)
  sDev <- 0
  for(i in 1:size)
  {
    d<-(data[i]-datamean)
    dataInput<-c(dataInput,d)
  }
  dataInput<-dataInput^3
  mean_dataInput<-UserMean(dataInput)
  sDev<-UserSD(data)
  sDev<-sDev^3
  skewness<-mean_dataInput/sDev
  return(skewness)
} # Measure of shape of given sample

UserKurtosis <-function(data){
      dataInput<-as.numeric()
      datamean<-UserMean(data)
      size<-length(data)
      for(i in 1:size)
      {
        d<-(data[i]-datamean)
        dataInput<-c(dataInput,d)
      }
      dataInput<-dataInput^4
      mean_dataInput<-mean(dataInput)
      sDev<-UserSD(data)
      sDev<-sDev^4
      kurtosis<-(mean_dataInput/sDev)-3   #subtraction of 3 is done so that mound shaped samples have values of g2 near zero.
      return(kurtosis)
} # Measures the degree of peakedness

################################################# Module 2 #############################################################################
UserCorrelation <-function(input,input1){
        sum1<-0
        dinp1<-as.numeric()
        mu1<-mean(input)
        n1<-length(input)
        for(i in 1:n1)
        {
          y<-(input[i]-mu1)
          dinp1<-c(dinp1,y)
        }
        dinp2<-as.numeric()
        mu2<-mean(input1)
        n2<-length(input1)
        for(i in 1:n2)
        {
          y<-(input1[i]-mu2)
          dinp2<-c(dinp2,y)
        }
        dinp<-dinp1*dinp2
        for(i in 1:n1)
        { sum1<-sum1+dinp[i]
        }
        
        s1<-UserSD(input)
        s2<-UserSD(input1)
        s<-s1*s2
        r<-sum1/(s*(n1-1))
        r
}                 

UserMultipleLinearRegression<-function(x1,x2,y,n){
        
        x1sq<-x1*x1
        x2sq<-x2*x2
        x1x2<-x1*x2
        x1y<-x1*y
        x2y<-x2*y
        x<-matrix(nrow=3,ncol=3)
        x[1,1]=n
        x[1,2]=sum(x1)
        x[1,3]=sum(x2)
        x[2,1]=sum(x1)
        x[2,2]=sum(x1sq)
        x[2,3]=sum(x1x2)
        x[3,1]=sum(x2)
        x[3,2]=sum(x1x2)
        x[3,3]=sum(x2sq)
        y1<-matrix(nrow=3,ncol=1)
        y1[1,1]=sum(y)
        y1[2,1]=sum(x1y)
        y1[3,1]=sum(x2y)
        coeff<-solve(x,y1)
        return(paste0("y= ",coeff[1,1]," + x1*",coeff[2,1]," + x2*",coeff[3,1]))
        
      } 
      


################################################# Module 3 #############################################################################

UserFactorial <- function(num){
    
    fact <- 1
    while(num > 0)
    {
        fact <- fact*num
        num <- num - 1
    }
    return(fact)
}

UserPermutation<-function(num1,num2){
  permutation <- 0
  permutation <- ((UserFactorial(num1))/(UserFactorial(num2)))
  return(permutation)
}

UserCombination<-function(num1,num2){
  combination <- 0
  combination <- ((UserFactorial(num1))/((UserFactorial(num2))*(UserFactorial(num1-num2))))
  return(combination)
}

UserBprobability <-function(sample,event){
  TotalOutcomes<-length(sample)
  PossibleOutcomes<-length(event)
  probability<-PossibleOutcomes/TotalOutcomes
  if(probability>1)
  {
    return("No such probability exists")
  }
  return(probability)
}

UserBayesT<-function(prior,like){
  sum<-0
  PABi <- prior * like
  for(i in 1:length(PABi))
  { 
    sum<-sum+PABi[i]
  }
  PBiA <- PABi/sum
  return(PBiA)
}

Intersection<-function(A,B,sample){
  out<-as.numeric()
  n1<-length(A)
  n2<-length(B)
  for(i in 1:n1)
  {
    for(j in 1:n2)
    {
      if(A[i] == B[j])
      { 
        y<-A[i]
        out<-c(out,y)
      }
    }
  }
  n<-length(out)
  if(n == 0)
  {
    PA <- UserBprobability(sample,A)
    PB <- UserBprobability(sample,B)
    inter <- PA*PB
    
  }
  else
  {
      inter  = UserBprobability(sample,n)
  }
    return(inter)
}

ConditionalProb1<-function(A,given,sample){
  PB<-UserBprobability(sample,given)
  PAB<-Intersection(A,given,sample)
  AB<-PAB/PB
  AB
}

ConditionalProb2<-function(B,given,sample){
  PB<-UserBprobability(sample,given)
  PAB<-Intersection(B,given,sample)
  BA<-PAB/PB
  BA
}

UserConditionalP <- function(A,B,sample){
  
  return(c(ConditionalProb1(A,B,sample),ConditionalProb2(B,A,sample)))
}


################################################# Module 4 #############################################################################


UserUniformDist<-function(x){
  x<-unique(x)
  n<-length(x)
  pdf<- 1/n
  return(pdf)
}

UserBernoulli<- function(theta,x){
  success<- (theta)^x
  failure <-(1-theta)^(1-x)
  pdf<-success*failure
  return(pdf)
}

UserBinomial<-function(x,n,theta){
  C<-UserCombination(n,x)              #number of ways in which we select x trials on which there is a success
  s<-theta^x                       #for x successes
  f<-(1-theta)^(n-x)               #for n-x failures
  pdf<-s*f  
  pdf = C*pdf  #since both are independent events
  return(pdf)
}

UserGeometric<-function(theta,x){
  failure<-(1-theta)^(x-1)
  pdf<-theta*failure
  return(pdf)
}

UserHypergeometric<-function(x,n,N,M){
  success<-UserCombination(M,x)         # number of way of choosing x of M succeses
  failure<-UserCombination(N-M,n-x)     # number of way of choosing (n-x) failures of (N-M) failuress
  total<-UserCombination(N,n)           # number of way of choosing n of N elements
  pdf<-(success*failure)/total
  return(pdf)
}

UserNegativeBinom<-function(k,x,theta){
  C<-UserCombination(x-1,k-1)               #number of way of choosing kth success on x trials
  s<-theta^k
  f<-(1-theta)^(x-k)
  pdf<-C*s*f                            #Probability of getting kth success on xth trial
  return(pdf)
}

UserPoisson<-function(n,theta,x){
  lambda<-n*theta
  fact<-UserFactorial(x)
  pdf<-(lambda^x*exp(-lambda))/fact
  return(pdf)
}

UserMultinomial<-function(n,xis,Pis){
  mul<-1
  div<-1
  fact<-UserFactorial(xis)
  n1<-length(fact)
  for(i in 1:n1)
  {
    div<-(div*fact[i])
  }
  nfact<-UserFactorial(n)
  d<-nfact/div
  pow<-Pis^xis
  for(i in 1:n1)
  {
    mul<-mul*pow[i]
  }
  pdf<-d*mul
  return(pdf)
}

UserMultivarHyperGeo<-function(xis,Mis,n){
  c<-1
  N1<-0
  C<-UserCombination(Mis,xis)
  for(i in 1:length(C))
  {
    c<-c*C[i]
  }
  for(i in 1:length(Mis))
  {
    N1<-N1+Mis[i]
  }
  TotalWays<-UserCombination(N1,n)
  pdf<-c/TotalWays
  return(pdf)
}

################################################# Module 5 #############################################################################
integrand <- function(y){
  al<-alpha-1
  (y^al)*(exp(-y))
}

gamma<-function(alpha){
  alpha <<- alpha
  gammaAlpha<-integrate(integrand, lower = 0, upper = Inf)
  return(gammaAlpha[[1]])
}

UserUniformContinous<-function(alpha,beta,lower,upper)
{
  alpha <<- alpha
    if(lower>alpha&&upper<beta)
  {
    diff<-beta-alpha
    pdf<-1/diff
  }
  else
  {
    pdf <- 0
  }
 
  f<-function(pdf)
  {
    pdf
  }
  val<-integrate(f,lower = lower, upper = upper)
  return(val)
  
}


UserGammaDist<-function(lower,upper,alpha,beta)
{
  if(lower>0&&upper>0)
  {
    div<-beta^alpha*gamma(alpha)
    mult<-(x^(alpha-1))*(exp((-x)/beta))
    pdf<-mult/div
  }
  else
  {
    pdf<-0
  }
  f<-function(pdf)
  {
    pdf
  }
  val<-integrate(f,lower=lower,upper=upper)
  return(val)
}


UserExpoDist<-function(theta,lower,upper)
{
  upper <<- upper
  if(lower > 0 && upper>0)
  {
    div<-theta*exp((-x)/theta)
    pdf<-1/div
    
  }
  else
  {
    pdf<-0
  }
  f<-function(pdf,x)
  {
    pdf
  }
  
  val<-integrate(f, lower  = lower, upper = upper)
  return(val)
}


UserNormalDist<-function(lower,upper,mu,sigma)
{
  d<-(x-mu)/sigma
  d<-0.5*(d^2)
  div<-sigma*sqrt(2*3.14)
  pdf<-(exp(-d))/div
  f<-function(pdf)
  {
    pdf
  }
  
  val<-integrate(f,lower=lower,upper=upper)
  return(val)
}


UserBivariateNormal<-function(xi,yi){
  rho<-UserCorrelation(xi,yi)
  mu1<-UserMean(xi)
  sigma1<-UserSD(xi)
  mu2<-UserMean(yi)
  sigma2<-UserSD(yi)
  d1<-(xi-mu1)/sigma1
  d2<-(yi-mu2)/sigma2
  d<-1/2*(1-rho^2)
  power<-d*((d1^2)-(2*rho*d1*d2)+d2^2)
  mul<-exp(-power)
  div<-2*3.14*sigma1*sigma2*sqrt(1-rho^2)
  pdf<-mul/div
  return(pdf)
}

################################################# Module 6 #############################################################################


UserZTest<- function(sigma,mu,n,alpha,xbar,case){
  z_cal-(xbar-mu)/(sigma/sqrt(n))  
  z_obs1<-qnorm(alpha)
  
  if(case==1)
  {
    if(z_cal<=qnorm(alpha/2) && z_cal>=qnorm(alpha/2))
       {
         return("Reject null hypothesis")
      }
    else
    {
      return("accept null hypothesis")
    }
  }
  else  if(case==2)
    {
      if(z_cal>=abs(qnorm(alpha)))
      {
        return("Reject null hypothesis")
      }
      else
      {
        return("accept null hypothesis")
      }
    }
    if(case==3)
      {
        if(z_cal<=qnorm(alpha))
        {
          return("Reject null hypothesis")
        }
        else
        {
          return("accept null hypothesis")
        }
        
      }
    
  
}

UserStudentTtest<-function(data,mu,alpha,case){
        n<-length(data)
        var<-UserVariance(data)
        xbar<-mean(data)
        t_cal<-(xbar-mu)/((var^0.5)/sqrt(n))
        
        if(case==1)  #two tail
        {
          if(t_cal<=qt(alpha/2,n-1) && t_cal>=qt(alpha/2,n-1))
             {
               return("Reject null hypothesis")
          }
          else
          {
            return("accept null hypothesis")
          }
        }
        if(case==2)       #right tail
        {
          if(t_cal>=abs(qt(alpha,n-1)))
          {
            return("Reject null hypothesis")
          }
          else
          {
            return("accept null hypothesis")
          }
        }
        if(case==3)                  #left tail
        {
          if(t_cal<=qt(alpha,n-1))
          {
            return("Reject null hypothesis")
          }
          else
          {
            return("accept null hypothesis")
          }
        }
}
      
UserChiSqTest<-function(data,popVar,alpha,case){
        case <<- case
        n<-length(data)
        sampVar<-UserVariance(data)
        chi_cal<-(n-1)*sampVar/popVar
        print(chi_cal)
        if(case==1)
        {
          if(chi_cal<=qchisq(alpha/2,n-1) && chi_cal>=qchisq((1-alpha/2),n-1))
          {
            return("Reject null hypothesis")
          }
          else
          {
            return("accept null hypothesis")
          }
        }
        if(case==2)
        {
          if(chi_cal >= qchisq((1-alpha),n-1))
          {
            return("Reject null hypothesis")
          }
          else
          {
            return("accept null hypothesis")
          }
        }
        if(case==3)
        {
          if(chi_cal <= qchisq(alpha,n-1))
          {
            return("Reject null hypothesis")
          }
          else
          {
            return("accept null hypothesis")
          }
        }
}
      
UserFDistTest<-function(data1,data2,alpha,case){
        Svar1<-UserVariance(data1)
        Svar2<-UserVariance(data2)
        n1<-length(data1)
        n2<-length(data2)
        f_cal1<-Svar1/Svar2
        f_cal2<-Svar2/Svar1
        if(case==1)               #two tail
        {
          if(Svar1>=Svar2)
          {
            if(f_cal1>=qf(1-(alpha/2),n1-1,n2-1))
            {
              return("Variances are not equal!")
            }
            else
            {
              return("Variances are equal")
            }
          }
          else
          {
            if(f_cal2>=qf(1-(alpha/2),n2-1,n1-1))
            {
              return("Variances are not equal!")
            }
            else
            {
              return("Variances are equal")
            }
          }
        }
        if(case==2)       #right tail
        {
          if(f_cal1>=qf(1-alpha,n1-1,n2-1))
            return("Variance of population1 is greater then Variance of population2")
          else
            return("Variances are equal")
        }
        if(case==3)
        {
          if(f_cal2>=qf(1-alpha,n2-1,n1-1))  #left tail
            return("Variance of population1 is less then Variance of population2")
          else
            return("Variances are equal")
        }
      }
      
      

################################################# Module 7 #############################################################################

UserMeanEst<-function(xi,alpha,psigma = 0,mu = 0){
  alpha<-alpha/2
  if(psigma == 0)
  {
    sigma<-UserSD(xi)
    xbar<-UserMean(xi)
    n<-length(xi)
    if(n>30)
    {
      
      z<-abs(qnorm(alpha))
      val<-(z*sigma)/(sqrt(n))
      CI<- c(LowerConfidenceLimit=xbar-val,UpperConfidenceLimit=xbar+val)
    }
    else
    {
      t<-abs(qt(alpha,n))
      val<-(t*sigma)/(sqrt(n))
      CI<- c(LowerConfidenceLimit=xbar-val,UpperConfidenceLimit=xbar+val)
    }
  }
  else{
    xbar = mu
    sigma = psigma
    z<-abs(qnorm(alpha))
    val<-(z*sigma)/(sqrt(n))
    CI<- c(LowerConfidenceLimit=xbar-val,UpperConfidenceLimit=xbar+val)
    
  }
  
  return(CI)
}  

UserDiffInMeanEst<-function(x1,x2,alpha,variance1 = 0,variance2 = 0,mu1= 0, mu2 = 0){
  if( variance1 == 0 && variance2 == 0)
  {
      x1bar<-UserMean(x1)
      x2bar<-UserMean(x2)
      var1<-UserVariance(x1)
      var2<-UserVariance(x2)
      alpha<<-alpha/2
      n1<<-length(x1)
      n2<<-length(x2)
      if(n1>=30&&n2>=30)
      {
        
        z<-abs(qnorm(alpha))
        val<-z*sqrt((var1/n1)+(var2/n2))
        diff<<-x1bar-x2bar
        CI<-c(LowerConfidenceLimit=diff-val,UpperConfidenceLimit=diff+val)
      }
      else
      {
        sp=((n1-1)*var1 +(n2-1)*var2)/(n1+n2-2)
        sp=sqrt(sp)
        t<-abs(qt(alpha,n1+n2-2)) 
        val<-t*sp*sqrt((1/n1)+(1/n2))
        return(c(LowerConfidenceLimit=(diff-val),UpperConfidenceLimit=(diff+val)))
      }
  }
  else{
    x1bar = mu1
    x2bar = mu2
    var1 = variance1
    var2 = variance2
    z<-abs(qnorm(alpha))
    val<-z*sqrt((var1/n1)+(var2/n2))
    diff<<-x1bar-x2bar
    CI<-c(LowerConfidenceLimit=diff-val,UpperConfidenceLimit=diff+val)
  }
  return(CI)
}

UserProportionEst<-function(percentCI,x,n){   
    theta<-x/n
    alpha<-1-(percentCI/100)
    alpha<-alpha/2
    z<-abs(qnorm(alpha))
    val<-z*sqrt((theta*(1-theta))/n)
    CI<-c(LowerConfidencelevel = theta-val,UpperConfidencelevel=theta+val)
    return(CI)
}

UserDiffInPropor<-function(x1,n1,x2,n2,percentCI){
  theta1<-x1/n1
  theta2<-x2/n2
  alpha<-1-(percentCI/100)
  alpha<-alpha/2
  z<-qnorm(alpha)
  z<-abs(z)
  val<-z*sqrt((theta1*(1-theta1))/n1+(theta2*(1-theta2))/n2)
  diff<-theta1-theta2
  CI<-c(Lower=diff-val,Upper=diff+val)
  return(CI)
}

UserEstVariance<-function(x,percentCI){
  alpha<-1-(percentCI/100)
  alpha<-alpha/2
  var<-UserVariance(x)
  n<-length(x)
  chi1<-qchisq((1-alpha),n-1)
  chi2<-qchisq(alpha,n-1)
  CI<-c(Lower=((n-1)*var)/chi1,Upper=((n-1)*var)/chi2)
  return(CI)
}

UserEstRatioVariance <- function(data1,data2,percentCI){
  percentCI <- percentCI/100
  alpha <- 1-percentCI
  alpha <- alpha/2
  
  n1 <- length(data1)
  n2 <- length(data2)
  
  s1 <- UserVariance(data1)
  s2 <- UserVariance(data2)
  
  fvalue1 <- qf(percentCI,n1-1,n2-1)
  fvalue2 <- qf(percentCI,n2-1,n1-1)
  
  val1 <- (s1/s2)*(1/fvalue1)
  val2 <- (s1/s2)*(fvalue2)
  CI = c(LowerConfidenceLimit=val1,UpperConfidenceLimit = val2)
  return(CI)
}

################################################# Module 8 #############################################################################
UserSignT <- function(data,mu,alpha){
  countn <- 0
  countp <- 0
  theta <- 1/2
  for(i in 1:length(data))
  {
    if( (data[i]-mu) < 0){
        countn <- countn + 1
    }
    else if((data[i]-mu) > 0){
      countp <- countp + 1
      
    }
  }
  size <- countn+countp
  if(size < 30){
    result <- UserBinomial(countp,size,theta)
  }
  else {
       num <- countp - (size*theta)
       denom <-sqrt(size*theta*(1-theta))
       result <- num/denom
  }
  if(result < 0)
  {
    result <- abs(result)
  }
  if(result < alpha)
    {
    return("Reject the Null Hypothesis ( Ho )")
  }
  else{
    return("Accept the Null Hypothesis ( Ho )")
    
  }
}

UserWilcoxonTest <- function(data,mu,alpha,case){
  tneg = 0
  tpos = 0
  count0 = 1
  data_minus_mu <- data - mu
  
  data_rank = SortRank(data_minus_mu)  
  
  for(i in 1:length(data_rank)){
    if( data_rank[i] == 0.0)
    {
      count0 = count0 + 1
    }
  }
  size <- length(data_rank)
  data_rank = data_rank[count0:size]
  
  for(i in 1:length(data_rank)){
    
    if(data_rank[i] < 0){
      tneg = tneg+i
    }
    else if (data_rank[i] > 0){
      tpos = tpos+i
    }
  }
  n = length(data_rank)
  t_min = min(tneg,tpos)
  if(case == 1)
  { 
    tvalue = qsignrank(alpha/2,n)
    
      if(t_min <= tvalue)
      {
        return("Null Hypothesis (Ho) is rejected")
      }
    else{
      return("Null Hypothesis (Ho) is accepted")
      
    }
  }
  else if(case == 2)
  {
    tvalue = qsignrank(alpha,n)
    
      if(tneg <= tvalue)
    {
      return("Null Hypothesis (Ho) is rejected")
    }
    else{
      return("Null Hypothesis (Ho) is accepted")
      
    }
  }
  else if(case == 3){
    tvalue = qsignrank(alpha,n)
    
    if(tpos <= tvalue)
    {
      
      return("Null Hypothesis (Ho) is rejected")
    }
    else{
      return("Null Hypothesis (Ho) is accepted")
      
    }
    }

}

UserMannWTest <- function(data1,data2,alpha,case){
  total_data = as.numeric()
  
  for(i in 1:length(data1)){
    total_data = c(total_data,d1=data1[i])
  }
  for(i in 1:length(data2)){
    total_data = c(total_data,d2=data2[i])
  }
  
  total_data = UserSortKruskal(total_data)
  total_data_rank= c(1)
  
  for(i in 2:length(total_data)){
    if( total_data[i] == total_data[i-1] )
    {
      avg = (i+i-1)/2
      total_data_rank[i-1]=avg
      total_data_rank[i]=avg
    }
    else
    {
      total_data_rank[i]=i
    }
  }
  W1 <-0
  W2 <-0
  
  for(i in 1:length(total_data_rank))
  {
    if(names(total_data[i]) == "d1")
    {
      W1 <- W1 + total_data_rank[i]
    }
    else if(names(total_data[i]) == "d2")
    {
      W2 <- W2 + total_data_rank[i]
    }
  }
  n1 = length(data1)
  n2 = length(data2)
  U1 = W1 - (n1*(n1+1))/2
  U2 = W2 - (n2*(n2+1))/2
  U_cal = min(U1,U2)
  if(case == 1)
  {
    U = qwilcox(alpha/2,n1,n2)-1
    if(U_cal <= U)
    {
      return("Reject Null Hypothesis (Ho) ")
    }
    else{
      return("Accept Null Hypothesis (Ho) ")
    }
  }
  else if(case == 2)
  {
    U = qwilcox(alpha,n1,n2)-1
    if(U2 <= U)
    {
      return("Reject Null Hypothesis (Ho) ")
    }
    else{
      return("Accept Null Hypothesis (Ho) ")
    }  
  }
  else if( case == 3)
  {
    U = qwilcox(alpha,n1,n2)-1
    if(U1 <= U)
    {
      return("Reject Null Hypothesis (Ho) ")
    }
    else{
      return("Accept Null Hypothesis (Ho) ")
    }
  }
} 

UserKruskalWTest <- function(data1,data2,data3=0,alpha){
    
    total_data = as.numeric()
    
    for(i in 1:length(data1)){
      total_data = c(total_data,d1=data1[i])
    }
    for(i in 1:length(data2)){
      total_data = c(total_data,d2=data2[i])
    }
    for(i in 1:length(data3)){
      total_data = c(total_data,d3=data3[i])
    }
    
    total_data = UserSortKruskal(total_data)
    total_data_rank= c(1)
    
    for(i in 2:length(total_data)){
      if( total_data[i] == total_data[i-1] )
      {
          avg = (i+i-1)/2
          total_data_rank[i-1]=avg
          total_data_rank[i]=avg
      }
      else
      {
        total_data_rank[i]=i
      }
    }
    R1 <-0
    R2 <-0
    R3 <-0
    
    for(i in 1:length(total_data_rank))
    {
        if(names(total_data[i]) == "d1")
        {
          R1 <- R1 + total_data_rank[i]
        }
        else if(names(total_data[i]) == "d2")
        {
          R2 <- R2 + total_data_rank[i]
        }
        else if(names(total_data[i]) == "d3")
        {
          R3 <- R3 + total_data_rank[i]
        }
      
    }
    R1 <- (R1*R1)/length(data1)
    R2 <- (R2*R2)/length(data2)
    R3 <- (R3*R3)/length(data3)
    n = length(total_data)
    H_calculated = (12/(n*(n+1)))*(R1+R2+R3) - 3*(n+1)
    H_observed = qchisq(1-alpha,n-1)
    if(H_calculated > H_observed)
    {
      return("Reject Null Hypothesis (Ho) ")
    }
    else{
      return("Accept Null Hypothesis (Ho) ")
    }
}

################################################# Module 9 #############################################################################

UserHist <- function(data){
  hist(data,xlab = "Weight",col = "red",border = "black",main = "Histogram")
}

UserLinegraph <- function(data,data1 = 0,data2 = 0){
    plot(data,type = "o", col = "red", xlab = "Month", ylab = "Drinks ",main = "Line Graph")
    lines(data1, type = "o", col = "blue")
    lines(data2, type = "o", col = "green")
}


UserBarplot <- function(data){
  barplot(data, col = "red", xlab = "Month", ylab = "Frequency",main = "Bar Graph")
}

UserPie <- function(data){
    pie(data,  main = "Pie chart")
}
    
UserScatter <- function(data,data1){
    plot(x = data,y = data1,
         xlab = "Wine",
         ylab = "Beer",		 
         main = "Scatter Diagram representation of Drink Types "
    )
}

UserBoxplot <- function(data,data1){
    boxplot(data~data1,
         ylab = "Drink",
         xlab = "Consumer",
         main = "Box Plot of Drink type with its consumer"
    )
}

UserQplot <- function(data,data1){
qqplot(x= data,y=data1,xlab = "Beer", ylab = "Spirit")
qqnorm(data1)
qqline(data1, col = "red")
}

UserStem <- function(data){
  stem(data)
}
