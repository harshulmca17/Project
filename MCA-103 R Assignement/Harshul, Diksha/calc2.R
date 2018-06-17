ReadInput <-function()
{j<-0
n<-readline("size")
x<-as.numeric()
while(j<n)
{

y<- as.numeric(readline("Enter data"))
x<-as.numeric(c(x,y))
j<-j+1
}
x
}

input<-ReadInput()

Length<<-function(input)
{
 i=0
 while(is.numeric(input[i]))
 {
  count=count+1
  i=i+1
 }
 
}
Length(input)

factorial<-function(num)
{ fact=1
 if(num==0)
 {
  fact=1
 }
 else
 {
  for(i in 1:num)
  {
   fact<-fact*i
  }
 fact
}
}
Sort<-function(input)
{
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
 input
}
Sort(input)


mean <-function(input)
{sum<-0
 input1<-is.na(input)
 input1<-as.numeric(input1)
 input2<-input[!input1]
 
 n<-length(input2)  
 for(i in 1:n)
 { sum<-sum+input2[i]
 }
 average<-sum/n
 average
}
 mean(input)

median <-function(input)
{
 input<-Sort(input)
 n<-length(input)
 if(n%%2==0)
 {
  n1<-n/2
  n2<-(n/2)+1
  median <-(input[n1]+input[n2])/2
 }
 else
 {
  n1<-(n+1)/2
  median<-input[n1]
 }
 median
}
median(input)

mode <-function(input)
{ tab_input<-table(input)
  tab_inputNames <-names(tab_input)[which.max(tab_input)]
  mode <-as.numeric(tab_inputNames)
  mode
}

Svariance <-function(input)
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

Svariance(input)

Pvariance <-function(input){
    sum1<-0
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
    var<-sum1/(n)
    var
}

Pvariance(input)


StdDev<-function(input)
{ 
 var<-variance(input)
 sigma <-var^(1/2)
 sigma
}

StdDev(input)

MAbsDev <-function(input)
{
      sum1<-0
     n<-length(input)
     a<-mean(input)
      for(i in 1:n)
      { y<-(input[i]-a)
        dinp<-c(dinp,y)
      }
     dinp<-abs(dinp)
     for(i in 1:n)
     { sum1<-sum1+dinp[i]
     }
     MAD<-sum1/(n-1)
     
     return(MAD)
}
 MAbsDev(input)

Range <-function(input)
{
 input<-Sort(input)
 min<-input[1]
 n<-length(input)
 max<-input[n]
 range<-max-min
 range
}

Range(input)


 Quartile <- function(input)
{
  input<- Sort(input)
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
Quartile(input)

InterQuartileRange<-function(input)
{
 quartile<-Quartile(input)
 q3<-quartile[3]
 q1<-quartile[1]
 iqr<-(q3-q1)/2
 iqr
}

InterQuartileRange(input)


Minimum <-function(input)
{
 input<-Sort(input)
 min<-input[1]
}

Minimum(input)

Maximum <-function(input)
{
 input<-Sort(input)
 n<-length(input)
 max<-input(n)
 max
}

Maximum(input)

Moments <-function(input)
{dinp<-as.numeric()
 mu <-mean(input)
 n<-length(input)
 for(i in 1:n)
 {
  y<-(input[i]-mu)
  dinp<-c(dinp,y)
 }
 mu1 <-mean(dinp)
 dinp1<-dinp^2
 mu2 <-mean(dinp1)

 dinp2<-dinp^3
 mu3 <-mean(dinp2)

 dinp3<-dinp^4
 mu4 <-mean(dinp3)

 moment<-c(mu1,mu2,mu3,mu4)
 names(moment)<-c("mu1","mu2","mu3","mu4")
 moment
}
Moments(input)

Skewness<-function(input)   # Measure of shape of given sample
{
 dinp<-as.numeric()
 mu<-mean(input)
 n<-length(input)
 for(i in 1:n)
 {
  y<-(input[i]-mu)
  dinp<-c(dinp,y)
 }
 dinp<-dinp^3
 mean_dinp<-mean(dinp)
 s<-StdDev(input)
 s<-s^3
 g1<-mean_dinp/s
 g1
}

Skewness(input)
 
Kurtosis <-function(input) # Measures the degree of peakedness
{
 dinp<-as.numeric()
 mu<-mean(input)
 n<-length(input)
 for(i in 1:n)
 {
  y<-(input[i]-mu)
  dinp<-c(dinp,y)
 }
 dinp<-dinp^4
 mean_dinp<-mean(dinp)
 s<-StdDev(input)
 s<-s^4
 g2<-(mean_dinp/s)-3   #subtraction of 3 is done so that mound shaped samples have values of g2 near zero.
 g2
}

Kurtosis(input)


input<-ReadInput()
input1<-ReadInput()
 
Correlation <-function(input,input1)
{
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

 s1<-StdDev(input)
 s2<-StdDev(input1)
 s<-s1*s2
 r<-sum1/(s*(n1-1))
 r
}

Correlation(input,input1)


MLR<-function()
{
 p<-as.numeric(readline("enter number of predictors:"))
 n<-as.numeric(readline("total values:"))
 p<-p+1
 nrow=n
 ncol=p
 n1<-nrow*ncol
 print("enter values of predictor(values of first row should be zero)")
 inp<-as.numeric()
 for(i in 1:n1)
 { 
  y<-as.numeric(readline("Enter data:"))
  inp<-c(inp,y)
 }
  xi<-matrix(inp,nrow=n,ncol=p,byrow=TRUE)
 nrow=p
 ncol=1
 n2<-nrow*ncol
 inp2<-as.numeric()
 print("enter value of coefficients of predictor")
 for(i in 1:n2)
 { 
  y<-as.numeric(readline("Enter data:"))
  inp2<-c(inp2,y)
 }
 coeff<-matrix(inp2,nrow=p,ncol=1)
 yi<-xi%*%coeff
 print("predicted values are:")
 yi
}
MLR()
  
 


n<-as.numeric(readline("Enter value of n:"))
r<-as.numeric(readline("Enter value of r:"))

Permutations <-function(n,r)
{#n<-length(input)
 nfact<-factorial(n)
 rfact<-factorial(r)
 perm<-nfact/rfact
 perm
}
 
Permutations(n,r)

Combination <-function(n,r)
{
 p<-Permutations(n,r)
 nrdiff<-n-r
 nrdiff_fact<-factorial(nrdiff)
 Comb<-p/nrdiff_fact
 Comb
}
 
Combination(n,r)

print("Enter the sample space")
sample = ReadInput()
print("Enter an event from the given sample space")
event = ReadInput()

BasicProbability<-function(sample,event)
{
 TotalOutcomes<-length(sample)
 PossibleOutcomes<-length(event)
 probability<-PossibleOutcomes/TotalOutcomes
 if(probability>1)
 {
  print("No such probability exists")
 }
 probability
}


BasicProbability(sample,event)

Intersection<-function(A,B,sample)
{
 out<-as.numeric()
 n1<-length(A)
 n2<-length(B)
 for(i in 1:n1)
 {
  for(j in 1:n2)
  {
   if(A[i]==B[j])
   { 
    y<-A[i]
    out<-c(out,y)
   }
  }
 }
 n<-length(out)
 BasicProbability(sample,n)
}

Intersection(input1,input2,sample)

sample=ReadInput()
input1=ReadInput()
input2=ReadInput()

ConditionalProb1<-function(A,given,sample)
{
 PB<-BasicProbability(sample,given)
 PAB<-Intersection(A,given,sample)
 AB<-PAB/PB
 AB
}

ConditionalProb1(input1,input2,sample)


ConditionalProb2<-function(B,given,sample)
{
 PB<-BasicProbability(sample,given)
 PAB<-Intersection(A,given,sample)
 BA<-PAB/PA
 BA
}

ConditionalProb2(input2,input1,sample)


 
 priorProb <- ReadInput()
 like <- ReadInput()

 BayesTh<-function(prior,likelihood)
{
 sum<-0
 PABi <- prior * like
 for(i in 1:length(PABi))
 { 
  sum<-sum+PABi[i]
 }
 PBiA <- PABi/sum
 PBiA
}

BayesTh(priorProb,like)



x<-ReadInput()

UnifDist<-function(x)
{ x<-unique(x)
 n<-length(x)
  pdf<- 1/n
  pdf
}

UnifDist(x)


prob<-as.numeric(readline("Enter probability of success:"))
x<-as.numeric(readline("success or failure that is 0 or 1:"))

Bernoulli<- function(theta,x)
{
 success<- (theta)^x
 failure <-(1-theta)^(1-x)
 pdf<-success*failure
 pdf
}

Bernoulli(prob,x)


theta<-as.numeric(readline("Enter probability of success:"))
n<-as.numeric(readline("Number of trials:"))
x<-as.numeric(readline("Number of successes:"))

Binomial<-function(x,n,theta)
{
 C<-Combination(n,x)              #number of ways in which we select x trials on which there is a success
 s<-theta^x                       #for x successes
 f<-(1-theta)^(n-x)               #for n-x failures
 pdf<-s*f                         #since both are independent events
 pdf
}

Binomial(x,n,theta)



theta<-as.numeric(readline("Enter probability of success:"))
x<-as.numeric(readline("Success on the trial:"))

Geometric<-function(theta,x)
{
 failure<-(1-theta)^(x-1)
 pdf<-theta*failure
 pdf
}

Geometric(theta,x)



N<-as.numeric(readline("Total number of elements:"))
n<-as.numeric(readline("Select number of trials from N elements:"))
M<-as.numeric(readline("Success from N elements:"))
x<-as.numeric(readline("Number of successes in n trial:"))

Hypergeometric<-function(x,n,N,M)
{
 success<-Combination(M,x)         # number of way of choosing x of M succeses
 failure<-Combination(N-M,n-x)     # number of way of choosing (n-x) failures of (N-M) failuress
 total<-Combination(N,n)           # number of way of choosing n of N elements
 pdf<-(success*failure)/total
 pdf
}
 
Hypergeometric(x,n,N,M)



k<-as.numeric(readline("number of success"))
x<-as.numeric(readline("Number of trials:"))
theta<-as.numeric(readline("Enter probability of success:"))

NegativeBinom<-function(k,x,theta)
{
 C<-Combination(x-1,k-1)               #number of way of choosing kth success on x trials
 s<-theta^k
 f<-(1-theta)^(x-k)
 pdf<-C*s*f                            #Probability of getting kth success on xth trial
 pdf
}

NegativeBinom(k,x,theta)



theta<-as.numeric(readline("Enter probability of success:"))
n<-as.numeric(readline("Number of trials(should be very large):"))

Poisson<-function(n,theta,x)         #Special case of Binomial Distribution when when number of trials are very large
{
 lambda<-n*theta
 fact<-factorial(x)
 pdf<-(lambda^x*exp(-lambda))/fact
 pdf
}

Poisson(n,theta,x)



n<-as.numeric(readline("total number of trials:"))
print("values of possible outcomes:")
xis<-ReadInput()
print("Probabilities of possible outcomes:")
Pis<-ReadInput()

Multinomial<-function(n,xis,Pis)
{
 mul<-1
 div<-1
 fact<-factorial(xis)
 n1<-length(fact)
 for(i in 1:n1)
 {
  div<-(div*fact[i])
 }
  nfact<-factorial(n)
  d<-nfact/div
  pow<-Pis^xis
 for(i in 1:n1)
 {
  mul<-mul*pow[i]
 }
 pdf<-d*mul
 pdf
}

Multinomial(n,xis,Pis)



n<-as.numeric(readline("number of trials:"))
print("values of possible outcomes:")
xis<-ReadInput()
print("Elements of kinds of possible outcomes:")
Mis<-ReadInput()

MultivarHyperGeo<-function(xis,Mis,n)      #Special case of Multinomial Distribution i.e. sampling is done without replacement
{
 c<-1
 N1<-0
 C<-Combination(Mis,xis)
 for(i in 1:length(C))
 {
  c<-c*C[i]
 }
 for(i in 1:length(Mis))
 {
  N1<-N1+Mis[i]
 }
 TotalWays<-Combination(N1,n)
 pdf<-c/TotalWays
 pdf
}

MultivarHyperGeo(xis,Mis,n)





x<-as.numeric(readline("Enter a random variable:"))
alpha<-as.numeric(readline("Enter a constant:"))
beta<-as.numeric(readline("Enter a constant(should be greater then alpha):"))
UniformC<-function(x,alpha,beta)
{
 
 if(x>alpha&&x<beta)
 {
  diff<-beta-alpha
  pdf<-1/diff
 }
 else
 {
 pdf<-0
 }
 pdf
}

UniformC(x,alpha,beta)




x<-as.numeric(readline("Enter a random variable:"))
alpha<<-as.numeric(readline("Enter a constant:"))
beta<-as.numeric(readline("Enter a constant(should be greater then alpha):"))

integrand <- function(y) 
{
 al<-alpha-1
  (y^al)*(exp(-y))
}
## integrate the function from 0 to infinity
gam<-function(alpha)
{gammaAlpha<-integrate(integrand, lower = 0, upper = Inf)
 return(gammaAlpha[[1]])
}
gam(alpha)
GammaDist<-function(x,alpha,beta)
{
 if(x>0)
 {
  div<-beta^alpha*gam(alpha)
  mult<-(x^(alpha-1))*(exp((-x)/beta))
  pdf<-mult/div
 }
 else
 {
  pdf<-0
 }
 pdf
}

GammaDist(x,alpha,beta)



theta<-as.numeric(readline("Enter:"))
x<-as.numeric(readline("Enter:"))

ExpoDist<-function(theta,x)
{
 if(x>0)
 {
 div<-theta*exp((-x)/theta)
 pdf<-1/div

}
 else
 {
  pdf<-0
 }
 pdf
}
ExpoDist(theta,x)




mu<-as.numeric(readline("Enter value of mean:"))
sigma<-as.numeric(readline("Enter value of standard deviation(should be greater then 0):"))
x<-as.numeric(readline("Enter value at which the density function is applied:"))

NormalDist<-function(x,mu,sigma)
{
 d<-(x-mu)/sigma
 d<-0.5*(d^2)
 div<-sigma*sqrt(2*3.14)
 pdf<-(exp(-d))/div
 pdf
}
 
NormalDist(x,mu,sigma)

xi<-ReadInput()
yi<-ReadInput()
BivariateNormal<-function(xi,yi)
{
 rho<-Correlation(xi,yi)
 mu1<-mean(xi)
 sigma1<-StdDev(xi)
 mu2<-mean(yi)
 sigma2<-StdDev(yi)
 d1<-(xi-mu1)/sigma1
 d2<-(yi-mu2)/sigma2
 d<-1/2*(1-rho^2)
 power<-d*((d1^2)-(2*rho*d1*d2)+d2^2)
 mul<-exp(-power)
 div<-2*3.14*sigma1*sigma2*sqrt(1-rho^2)
 pdf<-mul/div
 return(pdf)
}

BivariateNormal(xi,yi)

x<-as.numeric(readline("Enter a random variable:"))
dff<<-as.numeric(readline("Degree of Freedom:"))

ChiSquareDist<-function(x,dff)
{
 df1<-dff/2
 alpha<<-df1
 m<-x^((dff-2)/2)
 multiplicand<-m*exp((-x)/2)
 div<-gam(alpha)*2^df1
 pdf<-multiplicand/div
 pdf
}

ChiSquareDist(x,dff)


x<-as.numeric(readline("Enter a random variable:"))
r<<-as.numeric(readline("Degree of Freedom:"))

StudentTtest<-function(x,r)
{
 v<-(r+1)/2
 alpha<<-v
 gv<-gam(alpha)
 v1<-r/2
 alpha<<-v1
 gv1<-gam(alpha)
 x1<-x^2
 m<-(1+(x1/r))^(-(r+1)/2)
 mult<-gv*m
 div<-sqrt(3.14*r)*gv1
 pdf<-mult/div
 pdf
}

StudentTtest(x,r)

print("Enter a random variable having normal distribution:")
xi<-ReadInput()

ZTest<-function(xi)
{
 mu<-mean(xi)
 sigma<-StdDev(xi)
 diff<-xi-mu
 z<-diff/sigma
 return(z)
}

ZTest(xi)

xi<-ReadInput()
alpha<-as.numeric(readline("enter value of alpha:"))

MeanEst<-function(xi,alpha,psigma = 0,mu = 0)
{
  alpha<-alpha/2
  if(psigma == 0)
  {
       sigma<-StdDev(xi)
       xbar<-mean(xi)
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
       return(CI)
  }
  else{
    xbar = mu
    sigma = psigma
    z<-abs(qnorm(alpha))
    val<-(z*sigma)/(sqrt(n))
    CI<- c(LowerConfidenceLimit=xbar-val,UpperConfidenceLimit=xbar+val)
    
  }
}
MeanEst(xi,alpha)

x1<-ReadInput()
x2<-ReadInput()
alpha<-as.numeric(readline("enter value of alpha:")) 

DiffInMean<-function(x1,x2,alpha)
{
 x1bar-mean(x1)
 x2ba<-mean(x2)
 var1<-variance(x1)
 var2<-variance(x2)
 alpha<<-alpha/2
 n1<-length(x1)
 n2<-length(x2)
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
  CI<-c(LowerConfidenceLimit=diff-val,UpperConfidenceLimit=diff+val)
  }
 
 return(CI)
}

DiffInMean(x1,x2,alpha)


x<-as.numeric(readline("enter"))
n<-as.numeric(readline("enter"))
percentCI<-as.numeric(readline("Enter percentage at which confidence interval will be formed:"))

ProportionEst<-function(percentCI,x,n)
{ theta<-x/n
  alpha<-1-(percentCI/100)
  alpha<-alpha/2
  z<-abs(qnorm(alpha))
  val<-z*sqrt((theta*(1-theta))/n)
  CI<-c(Lower=theta-val,Upper=theta+val)
  return(CI)
}

ProportionEst(percentCI,x,n)



DiffInpropor<-function(x1,n1,x2,n2,percentCI)
{
 theta1<-x1/n1
 theta2<-x2/n2
 alpha<-1-(percentCI/100)
 alpha<-alpha/2
 z<-qnorm(alpha)
 z<-abs(z)
 val<-z*sqrt((theta1*(1-theta1))/n1+(theta2*(1-theta2))/n2)
 diff<-theta1-theta2
 CI<-c(Lower=diff-val,Upper=diff+val)
 CI
}

DiffInpropor(x1,n1,x2,n2,percentCI)


print("Enter a random sample from normal population")
x<-ReadInput()
percentCI<-as.numeric(readline("Enter percentage at which confidence interval will be formed:"))

EstVariance<-function(x,percentCI)
{
 alpha<-1-(percentCI/100)
  alpha<-alpha/2
  var<-variance(x)
  n<-length(x)
  chi1<-qchisq((1-alpha),n-1)
  chi2<-qchisq(alpha,n-1)
  CI<-c(Lower=((n-1)*var)/chi1,Upper=((n-1)*var)/chi2)
  return(CI)
}

EstVariance(x,percentCI)

