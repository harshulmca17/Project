library(shiny)
shinyUI(fluidPage(
  titlePanel("Assignment 3 ( MCA-103 )"),
  sidebarLayout( position = "left",
    
    sidebarPanel(
      fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
      helpText("Default max. file size is 5MB"),
      tags$hr(),
      selectInput("Module","Choose One Module ", c("Choose","Descriptive Analysis","Predective Analysis","Probability Analysis","Discrete Distribution Function","Continuous Distributive Function","sample Distributive Test Analysis","Interval Estimation","Non-Parametric Analysis","Visualization"),selected = "Choose"),
      uiOutput("vx")
      ),
    mainPanel(
      uiOutput("tb")
      
        )
    
  ),sidebarLayout( 
                   
                   sidebarPanel(
                     
                      uiOutput("file1"),
                      uiOutput("file2"),
                      uiOutput("file3"),
                      uiOutput("inp1"),
                      uiOutput("inp2"),
                      uiOutput("inp3"),
                      uiOutput("inp3a"),
                      uiOutput("inp3b"),
                      uiOutput("inp4"),
                      uiOutput("inp4a"),
                      uiOutput("inp4b"),
                      uiOutput("inp4c"),
                      uiOutput("inp5"),
                      uiOutput("inp6"),
                      uiOutput("inp7"),
                      uiOutput("inp8"),
                      uiOutput("inp8a"),
                      uiOutput("inp9"),
                      uiOutput("inp10"),
                      uiOutput("inp11"),
                      uiOutput("inp12"),
                      uiOutput("inp13"),
                      uiOutput("inp14"),
                      uiOutput("inp15"),
                      uiOutput("inp16"),
                      uiOutput("inp17"),
                      uiOutput("inp18"),
                      uiOutput("inp19"),
                      uiOutput("inp20"),
                      uiOutput("inp21"),
                      uiOutput("inp21a"),
                      uiOutput("inp21b"),
                      uiOutput("inp22"),
                      uiOutput("inp23"),
                      uiOutput("inp24"),
                      uiOutput("inp25"),
                      uiOutput("inp25a"),
                      uiOutput("inp25b"),
                      uiOutput("inp25c"),
                      uiOutput("inp25d"),
                      uiOutput("inp26"),
                      uiOutput("inp26a"),
                      uiOutput("inp26b"),
                      uiOutput("inp27"),
                      uiOutput("inp28"),
                      uiOutput("inp29"),
                      uiOutput("inp30"),
                      uiOutput("inp31"),
                      uiOutput("inp31a"),
                      uiOutput("inp31b") 
                      
                     
                   ),
                   mainPanel(
                     
                   )
                   
  )
  
  
  
))