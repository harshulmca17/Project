library(shiny)
source("MyStatsCalci.R")
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
mydata<<-0
shinyServer(function(input,output){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=",", header = TRUE, stringsAsFactors = FALSE)
    })
  data12 <- function(){
    file1 <- input$file
    if(is.null(file1)){return()} 
    mydata<<-read.csv(file=file1$datapath, sep=",", header = TRUE, stringsAsFactors = FALSE)
    return(mydata)
  }
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  var = c("Choose")
  var <- reactive({
    switch(input$Module,
           "Descriptive Analysis" = c("Mean", "Median", "Mode", "Variance", "Standard Deviation", "Mean Absolute Deviation", "Range", "Quartiles", "IQR", "Minimum", "Maximum", "Skewness", "Kurtosis", "Moments"),
           "Predective Analysis" = c("Correlation", "Multiple Linear Regression"),
           "Probability Analysis" = c("Permutations", "Combinations"," Basic Probability", "Conditional Probability"," Bayes Theorem"),
           "Discrete Distribution Function" = c("Uniform"," Bernoulli"," Binomial", "Geometric","Hyper-geometric"," Negative Binomial"," Poisson"," Multinomial"," Multivariate Hypergeometric"),
           "Continuous Distributive Function" = c("UniformC", "Normal", "Bivariate Normal", "Gamma","Exponential"),
           "sample Distributive Test Analysis" = c("Chi-Square"," Student t-test"," F-test"," Z-test"),
           "Interval Estimation" = c("Estimation of Means", "Estimation of Differences in Means"," Estimation of Proportions"," Estimation of Differences in Proportions"," Estimation of Variances"," Estimation of  Ratio of Two Variances"),
           "Non-Parametric Analysis" = c("Sign Test"," Wilcoxon Signed-Rank test"," Mann-Whitney Test"," Kruskal-Wallis Test"),
           "Visualization" = c("Histograms"," Line Graph"," Bar Graph"," Pie Chart","Scatter plot","Box-plot","q-q plot","Stem-leaf plot")
           
           
    )
  })
###################################################### INPUTS ACCORDING TO FUNCTION ########################################
  output$vx <- renderUI({
    selectInput("variablex", "Select Function", choices = var())
  })
############################################ MODULE 1 INPUT #################################################################################  
  output$file1 <- renderUI({
    if(input$Module == "Descriptive Analysis"  ){
      name = names(data12())
      name = name[2:5]
      selectInput("variabley", "Select one columne", choices = name)
    }
    })
  
############################################ MODULE 2 INPUT #################################################################################  
  
  output$inp15 <- renderUI({
    if(input$Module == "Predective Analysis" ){
      textInput("pi15","Enter the 1st input : ","Data frame 1")
    }
    })
  
  output$inp16 <- renderUI({
    if(input$Module == "Predective Analysis"  ){
      textInput("pi16","Enter the 2nd input : ","Data Frame 2")
    }

  })
  
  output$inp17 <- renderUI({
    if((input$Module == "Predective Analysis") && (input$variablex == "Multiple Linear Regression")){
      textInput("pi17","Enter the 3rd input : ","Data Frame 3")
    }

  })
  
  output$inp18 <- renderUI({
    if((input$Module == "Predective Analysis") && (input$variablex == "Multiple Linear Regression")){
      numericInput("pi18","Total Number  : ",0)
    }
  })
  m2d1 <- reactive(as.numeric(unlist(strsplit(input$pi15,","))))
     
    m2d2 <- reactive(as.numeric(unlist(strsplit(input$pi16,","))))
    m2d3 <- reactive(as.numeric(unlist(strsplit(input$pi17,","))))
    
  
  
    
  
############################################ MODULE 3 INPUT ##################################################################################
  output$inp1 <- renderUI({
    if(input$Module == "Probability Analysis" && (input$variablex == "Permutations" || input$variablex == "Combinations" )){
      numericInput("pi1","Enter the 1st input : ",0)
    }
  })
  output$inp2 <- renderUI({
    if(input$Module == "Probability Analysis"  && (input$variablex == "Permutations" || input$variablex == "Combinations" )){
      numericInput("pi2","Enter the 2nd input : ",0)
    }
  })
  output$inp3 <- renderUI({
    if((input$Module == "Probability Analysis") && (input$variablex == "Conditional Probability" || input$variablex == " Bayes Theorem" || input$variablex == " Basic Probability" )){
      textInput("pi3","Enter the 1st : ","First Vector")
    }
  })
  output$inp3a <- renderUI({
    if((input$Module == "Probability Analysis") && (input$variablex == "Conditional Probability" || input$variablex == " Bayes Theorem" || input$variablex == " Basic Probability" )){
      textInput("pi3a","Enter the 2nd input : ","Second Vector")
    }
  })
  output$inp3b <- renderUI({
    if((input$Module == "Probability Analysis") && (input$variablex == "Conditional Probability")){
      textInput("pi3b","Enter the 3rd input : ","Third Vector")
    }
  })
  m3d1 <- reactive(as.numeric(unlist(strsplit(input$pi3,","))))
  m3d2 <- reactive(as.numeric(unlist(strsplit(input$pi3a,","))))
  m3d3 <- reactive(as.numeric(unlist(strsplit(input$pi3b,","))))
  
  
  
############################################ MODULE 4 INPUT #################################################################################
  
  output$inp4 <- renderUI({
    if(input$Module == "Discrete Distribution Function" && input$variablex == "Uniform"){
      textInput("pi4","Enter data : ")
    }
  })
  output$inp4b <- renderUI({
    if(input$Module == "Discrete Distribution Function" &&(input$variablex == " Multinomial"|| input$variablex ==" Multivariate Hypergeometric")){
      textInput("pi4b","Enter data 2 : ",0)
    }
  })
  output$inp4c <- renderUI({
    if(input$Module == "Discrete Distribution Function" &&(input$variablex == " Multinomial"|| input$variablex ==" Multivariate Hypergeometric") ){
      textInput("pi4c","Enter data 3 : ",0)
    }
  })
  
  
  output$inp5 <- renderUI({
    if(input$Module == "Discrete Distribution Function" && (input$variablex != "Uniform" && input$variablex != " Multinomial" && input$variablex !=" Multivariate Hypergeometric")){
      numericInput("pi5","Enter theta / n : ",0)
    }
  })
  output$inp4a <- renderUI({
    if(input$Module == "Discrete Distribution Function" && input$variablex != "Uniform"){
      numericInput("pi4a","Enter x : ",0)
    }
  })
  output$inp6 <- renderUI({
    if(input$Module == "Discrete Distribution Function" && (input$variablex == "Hyper-geometric" || input$variablex == " Binomial" || input$variablex == " Negative Binomial" || input$variablex == " Poisson")){
      numericInput("pi6","Enter N / K : ",0)
    }
  })
  output$inp7 <- renderUI({
    if(input$Module == "Discrete Distribution Function" && input$variablex == "Hyper-geometric"){
      numericInput("pi7","Enter M : ",0)
    }
  })
  m4d1 <- reactive(as.numeric(unlist(strsplit(input$pi4,","))))
  m4d2 <- reactive(as.numeric(unlist(strsplit(input$pi4b,","))))
  m4d3 <- reactive(as.numeric(unlist(strsplit(input$pi4c,","))))
  
############################################ MODULE 5 INPUT #################################################################################
  output$inp25 <- renderUI({
    if(input$Module == "Continuous Distributive Function" && input$variablex != "Bivariate Normal"  ){
      numericInput("pi25","Enter Lower : ",0)
    }
  })
  output$inp26 <- renderUI({
    if(input$Module == "Continuous Distributive Function" && input$variablex != "Bivariate Normal"   ){
      numericInput("pi26","Enter Upper : ",0)
    }
  })
  output$inp27 <- renderUI({
    if(input$Module == "Continuous Distributive Function" &&  ( input$variablex == "UniformC" || input$variablex == "Gamma")){
      numericInput("pi27","Enter alpha : ",0)
    }
  })
  output$inp25a <- renderUI({
    if(input$Module == "Continuous Distributive Function" &&  ( input$variablex == "UniformC" || input$variablex == "Gamma")){
      numericInput("pi25a","Enter beta : ",0)
    }
  })
  output$inp25b <- renderUI({
    if(input$Module == "Continuous Distributive Function" && input$variablex =="Exponential" ){
      numericInput("pi25b","Enter theta : ",0)
    }
  })
  output$inp25c <- renderUI({
    if(input$Module == "Continuous Distributive Function" && input$variablex =="Normal" ){
      numericInput("pi25c","Enter mu : ",0)
    }
  })
  output$inp25d <- renderUI({
    if(input$Module == "Continuous Distributive Function" && input$variablex =="Normal"){
      numericInput("pi25d","Enter sigma : ",0)
    }
  })
  output$inp26a <- renderUI({
    if(input$Module == "Continuous Distributive Function" && input$variablex == "Bivariate Normal" ){
      textInput("pi26a","Enter data 1 : ",0)
    }
  })
  output$inp26b <- renderUI({
    if(input$Module == "Continuous Distributive Function" && input$variablex == "Bivariate Normal" ){
      textInput("pi26b","Enter data 2 : ",0)
    }
  })
  m5d1 <- reactive(as.numeric(unlist(strsplit(input$pi26a,","))))
  m5d2 <- reactive(as.numeric(unlist(strsplit(input$pi26b,","))))
  
############################################ MODULE 6 INPUT #################################################################################
  output$inp19 <- renderUI({
    if(input$Module == "sample Distributive Test Analysis"){
      numericInput("pi9","Enter alpha : ",0)
    }
  })
  output$inp20<- renderUI({
    if(input$Module == "sample Distributive Test Analysis"){
      numericInput("pi20","Enter case (1,2,3 only) : ",0)
    }
  })
  
  output$inp21 <- renderUI({
    if(input$Module == "sample Distributive Test Analysis" && (input$variablex =="Chi-Square" ||input$variablex == " Student t-test" ||  input$variablex == " Z-test") ){
      numericInput("pi21","Enter mu/popVar : ",0)
    }
  })
  output$inp22 <- renderUI({
    if(input$Module == "sample Distributive Test Analysis" && input$variablex == " Z-test"){
      numericInput("pi22","Enter n : ",0)
    }
  })
  output$inp23 <- renderUI({
    if(input$Module == "sample Distributive Test Analysis" && input$variablex == " Z-test"){
      numericInput("pi23","Enter xbar : ",0)
    }
  })
  output$inp24 <- renderUI({
    if(input$Module == "sample Distributive Test Analysis" && input$variablex == " Z-test"){
      numericInput("pi24","Enter sigma: ",0)
    }
  })
  output$inp21a <- renderUI({
    if(input$Module == "sample Distributive Test Analysis" && (input$variablex == "Chi-Square"||input$variablex == " Student t-test" ||  input$variablex == " F-test") ){
      textInput("pi21a","Enter vector 1 : ")
    }
  })
  output$inp21b <- renderUI({
    if(input$Module == "sample Distributive Test Analysis" && (input$variablex == " F-test") ){
      textInput("pi21b","Enter vector 2 : ")
    }
  })
  m6d1 <- reactive(as.numeric(unlist(strsplit(input$pi21a,","))))
  m6d2 <- reactive(as.numeric(unlist(strsplit(input$pi21b,","))))
  
############################################ MODULE 7 INPUT #################################################################################
  output$inp8 <- renderUI({
    if(input$Module == "Interval Estimation" && ( input$variablex != " Estimation of Proportions" && input$variablex !=" Estimation of Variances" && input$variablex !=" Estimation of Differences in Proportions")){
      textInput("pi8","Enter data 1 : ",0)
    }
  })
  output$inp9 <- renderUI({
    if(input$Module == "Interval Estimation" && ( input$variablex ==" Estimation of  Ratio of Two Variances" || input$variablex =="Estimation of Differences in Means" || input$variablex ==" Estimation of Variances")  ){
      textInput("pi9","Enter data 2 : ",0)
    }
  })
  output$inp10 <- renderUI({
    if(input$Module == "Interval Estimation" && ( input$variablex ==  "Estimation of Means"|| input$variablex ==  "Estimation of Differences in Means")){
      numericInput("pi10","Enter alpha : ",0)
    }
  })
  output$inp11 <- renderUI({
    if(input$Module == "Interval Estimation"&& ( input$variablex !=  "Estimation of Means" && input$variablex !=  "Estimation of Differences in Means")){
      numericInput("pi11","Enter Percent CI : ",0)
    }
  })
  output$inp12 <- renderUI({
    if(input$Module == "Interval Estimation" && (input$variablex ==" Estimation of Differences in Proportions" || input$variablex ==  "Estimation of Means"|| input$variablex ==  "Estimation of Differences in Means" )){
      numericInput("pi12","Enter Variance / Pop Sigma (leave for Unknown Case) : ",0)
    }
  })
  output$inp13 <- renderUI({
    if(input$Module == "Interval Estimation" && ( input$variablex ==  "Estimation of Means"|| input$variablex ==  "Estimation of Differences in Means" ||input$variablex ==" Estimation of Differences in Proportions")){
      numericInput("pi13","Enter mu1 (leave for Unknown Case): ",0)
    }
  })
  output$inp14 <- renderUI({
    if(input$Module == "Interval Estimation" && ( input$variablex =="Estimation of Differences in Means"|| input$variablex ==" Estimation of Proportions"|| input$variablex ==" Estimation of Differences in Proportions")){
      numericInput("pi14","Enter Variance(leave for Unknown Case) / X : ",0)
    }
  })
  output$inp8a <- renderUI({
    if(input$Module == "Interval Estimation" && ( input$variablex =="Estimation of Differences in Means"|| input$variablex ==" Estimation of Proportions"|| input$variablex ==" Estimation of Differences in Proportions")){
      numericInput("pi8a","Enter mu2 (leave for Unknown Case)/ N  : ",0)
    }
  })
  m7d1 <- reactive(as.numeric(unlist(strsplit(input$pi8,","))))
  m7d2 <- reactive(as.numeric(unlist(strsplit(input$pi9,","))))
  
    
  
  
############################################ MODULE 8 INPUT #################################################################################
  output$inp28 <- renderUI({
    if(input$Module == "Non-Parametric Analysis" ){
    textInput("pi28","Enter data 1 : ",0)
    }
  })
  output$inp29 <- renderUI({
    if(input$Module == "Non-Parametric Analysis" && (input$variablex == " Mann-Whitney Test"|| input$variablex ==" Kruskal-Wallis Test" ) ){
      textInput("pi29","Enter data 2 : ",0)
    }
  })
  output$inp30 <- renderUI({
    if(input$Module == "Non-Parametric Analysis" && input$variablex ==" Kruskal-Wallis Test" ){
    textInput("pi30","Enter data 3 : ",0)
    }
  })
  output$inp31 <- renderUI({
    if(input$Module == "Non-Parametric Analysis" ){
    numericInput("pi31","Enter alpha : ",0)
    }
  })
  output$inp31a <- renderUI({
    if(input$Module == "Non-Parametric Analysis" && (input$variablex =="Sign Test" || input$variablex ==" Wilcoxon Signed-Rank test" )){
      numericInput("pi31a","Enter mu : ",0)
    }
  })
  output$inp31b <- renderUI({
    if(input$Module == "Non-Parametric Analysis" && (input$variablex == " Mann-Whitney Test" || input$variablex ==" Wilcoxon Signed-Rank test" )){
      numericInput("pi31b","Enter case : ",0)
    }
  })
  m8d1 <- reactive(as.numeric(unlist(strsplit(input$pi28,","))))
  m8d2 <- reactive(as.numeric(unlist(strsplit(input$pi29,","))))
  m8d3 <- reactive(as.numeric(unlist(strsplit(input$pi30,","))))
  
############################################ MODULE 9 INPUT ###################################################################################
  output$file2 <- renderUI({
    if(input$Module ==  "Visualization"    ){
      name = names(data12())
      name = name[2:5]
      selectInput("fi2", "Select 1nd columne", choices = name)
    }
  })
  output$file3 <- renderUI({
    if(input$Module ==  "Visualization" && (input$variablex =="Scatter plot" ||input$variablex =="Box-plot"||input$variablex =="q-q plot")   ){
      name = names(data12())
      name = name[2:5]
      selectInput("fi3", "Select 2nd columne", choices = name)
    }
  })
    
############################################################################################################################  
  v <- reactive({
    switch (input$variabley,
            "beer_servings" = as.numeric(mydata$beer_servings) ,
            "wine_servings" = mydata$wine_servings,
            "spirit_servings"  = data12()$spirit_servings,
            "total_litres" = data12()$total_litres
      
    )
  })
  v1 <- reactive({
    switch (input$fi2,
            "beer_servings" = as.numeric(mydata$beer_servings) ,
            "wine_servings" = mydata$wine_servings,
            "spirit_servings"  = data12()$spirit_servings,
            "total_litres" = data12()$total_litres
            
    )
  })
  v2 <- reactive({
    switch (input$fi3,
            "beer_servings" = as.numeric(mydata$beer_servings) ,
            "wine_servings" = mydata$wine_servings,
            "spirit_servings"  = data12()$spirit_servings,
            "total_litres" = data12()$total_litres
            
    )
  })

  func <- reactive({
    switch(input$variablex,
############################################ Module 1 Function Call############################################################  
       "Mean" = UserMean(v()),
       "Median" = UserMedian(v()),
       "Mode" = UserMode(v()),
       "Variance" = UserVariance(v()),
       "Standard Deviation" = UserSD(v()),
       "Mean Absolute Deviation" = UserMeanAD(v()),
       "Range" = UserRange(v()),
       "Quartiles" = UserQuartile(v()) ,
       "IQR" = UserIQR(v()),
       "Minimum" = UserMinimum(v()),
       "Maximum" = UserMaximum(v()),
       "Skewness" = UserSkewness(v()),
       "Kurtosis" = UserKurtosis(v()),
       "Moments" = UserMoments(v()),
############################################ Module 2 Function Call####################################################################
      "Correlation" = UserCorrelation(m2d1(),m2d2()),
      "Multiple Linear Regression" = UserMultipleLinearRegression(m2d1(),m2d2(),m2d3(),input$pi18),
############################################ Module 3 Function Call####################################################################
      "Permutations" = UserPermutation(input$pi1,input$pi2),
      "Combinations" = UserCombination(input$pi1,input$pi2),
      " Basic Probability" = UserBprobability(m3d1(),m3d2()),
      "Conditional Probability" = UserConditionalP(m3d1(),m3d2(),m3d3()),
      " Bayes Theorem" = UserBayesT(m3d1(),m3d2()),        
############################################ Module 4 Function Call####################################################################
      "Uniform" = UserUniformDist(m4d1()),
      " Bernoulli" = UserBernoulli(input$pi5,input$pi4a),
      " Binomial" = UserBinomial(input$pi4a,input$pi6,input$pi5),
      "Geometric" = UserGeometric(input$pi5,input$pi4a), 
      "Hyper-geometric" = UserHypergeometric(input$pi4a,input$pi5,input$pi6,input$pi7),
      " Negative Binomial" = UserNegativeBinom(input$pi6,input$pi4a,input$pi5),
      " Poisson" = UserPoisson(input$pi4a,input$pi5,input$pi6),
      " Multinomial" = UserMultinomial(input$pi4a,m4d2(),m4d3()),
      " Multivariate Hypergeometric" = UserMultivarHyperGeo(m4d2(),m4d3(),input$pi4a),

############################################ Module 5 Function Call####################################################################

      "UniformC" = UserUniformContinous(input$pi27,input$pi25a,input$pi25,input$pi26),
      "Normal" = UserNormalDist(input$pi25,input$pi26,input$pi25c,input$pi25d),
      "Bivariate Normal" = UserBivariateNormal(m5d1(),m5d2()),
      "Gamma" = UserGammaDist(input$pi25,input$pi26,input$pi27,input$pi25a),
      "Exponential" = UserExpoDist(input$pi25b,input$pi25,input$pi26),
############################################ Module 6 Function Call####################################################################
      "Chi-Square" = UserChiSqTest(m6d1(),input$pi21,input$pi19,input$pi20),
      " Student t-test" = UserStudentTtest(m6d1(),input$pi21,input$pi19,input$pi20),
      " F-test" = UserFDistTest(m6d1(),m6d2(),input$pi19,input$pi20),
      " Z-test" = UserZTest(input$pi24,input$pi21,input$pi22,input$pi19,input$pi23,input$pi20),
      
      
############################################ Module 7 Function Call####################################################################

      "Estimation of Means" = UserMeanEst(m7d1(),input$pi10,input$pi12,input$pi13),
      "Estimation of Differences in Means" = UserDiffInMeanEst(m7d1(),m7d2(),input$pi10,input$pi12,input$pi13,input$pi14,input$pi8a),
      " Estimation of Proportions" = UserProportionEst(input$pi11,input$pi14,input$pi8a),
      " Estimation of Differences in Proportions" = UserDiffInPropor(input$pi12,input$pi13,input$pi14,input$pi8a,input$pi11),
      " Estimation of Variances" = UserEstVariance(input$pi12,input$pi11),
      " Estimation of  Ratio of Two Variances" = UserEstRatioVariance(m7d1(),m7d2(),input$pi11),
############################################ Module 8 Function Call####################################################################
    "Sign Test" = UserSignT(m8d1(),input$pi31a,input$pi31),
    " Wilcoxon Signed-Rank test" = UserWilcoxonTest(m8d1(),input$pi31a,input$pi31,input$pi31b),
    " Mann-Whitney Test" = UserMannWTest(m8d1(),m8d2(),input$pi31,input$pi31b),
    " Kruskal-Wallis Test" = UserKruskalWTest(m8d1(),m8d2(),m8d3(),input$pi31)
    
####################################################################################################333      
  
   )

    
  })
  output$func1 <- renderPlot({
    switch(input$variablex,
  ############################################ Module 9 Function Call####################################################################
  "Histograms" = UserHist(v1()),
  " Line Graph" = UserLinegraph(v1()),
  " Bar Graph" = UserBarplot(v1()),
  " Pie Chart" = UserPie(v1()),
  "Scatter plot" = UserScatter(v1(),v2()),
  "Box-plot" = UserBoxplot(v1(),v2()),
  "q-q plot" = UserQplot(v1(),v2()),
  "Stem-leaf plot" = UserStem(v1())
    )})
  submitButton("Calculate")
      # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Created by Diksha Verma and Harshul Kumar", heigth=200, width=200)
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data Preview", tableOutput("table")),tabPanel("Summary", tableOutput("sum")),tabPanel("Result is", func()),tabPanel("Graph is", if(!(is.null(v1()))){plotOutput("func1")}))
  })
})