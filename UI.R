####shiny::runGitHub("OneSidedT","markdunning")

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("One-Sample Tests"),
  
  sidebarPanel(
    h2("Data Import Parameters"),
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 selected=","),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 selected='"'),
    textInput("skip", "Number of rows to skip in data file before reading data",value=0),
    textInput("dataCol", "Column containing data to test",value=2),
    
    br(),    
    h2("Analysis Parameters"),
    checkboxInput("default.bins",label="Use Default Histogram Bin Width",value=TRUE),
    sliderInput("bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30),
    
    helpText("Use the histograms and boxplot to judge whether you need to use a parametric, or non-parametric test"),
    checkboxInput("do.parametric",label = "Use Parametric Test?",value = TRUE),
    helpText("Alternatively, you could choose to transform the data prior to statistical testing..."),
    radioButtons("transform","Transformation",c("None"="none","Log10"="log.10","Log2"="log.2","Natural Log"="log"),"none"),
    textInput("mu","True mean",
                value = 0),
    checkboxInput("showMu","Show true mean?", T),
    br(),
    radioButtons("alternative", "Alternative", c("Two-sided"="two.sided", "Greater" = "greater", "Lower"="less"),"two.sided"),
    br(),    
    h2("Report Parameters"),
    #submitButton ('Generate R Code', icon('toggle-right'))
    textInput("outfile", "What to call the output R script",value="analysis"),
    textInput("name", "Your Name",value="Anon."),
    textInput("title", "What title to use in the report",value="My R Analysis")
    ),
  
  mainPanel(
    tabsetPanel(
#      tabPanel("Plot",plotOutput("plot")),
      tabPanel("About",helpText("This app was developed by the Bioinformatics Core of Cancer Research Uk Cambridge Institute to accompany a training course. On the course webpage you will find lecture notes from the course and practical exercises that use this app"),
               a("Introduction to Statistical Analysis",href="http://bioinformatics-core-shared-training.github.io/IntroductionToStats/"),
               br(),
               helpText(),
               br(),
               br(),
               img(src="cruk-cambridge-institute.jpg",width=350,height=77), br(),a("cruk.cam.ac.uk",href="www.cruk.cam.ac.uk"),
               br(),
               br(),
               a("View source Code for app", href="https://github.com/bioinformatics-core-shared-training/OneSampleTest.git")),
      tabPanel("The data", dataTableOutput("mytable")),
#      tabPanel("Boxplot",plotOutput("boxplot")),
#      tabPanel("Histogram",plotOutput("histogram")),

#      tabPanel("Summary Statistics",
#               h4("Screen output in R"),
#               verbatimTextOutput("summary")),
tabPanel("Data Distribution", helpText("The boxplot and histogram of the data are shown below"),
         plotOutput("boxplot"),
         helpText("Numerical summary of the data..."),
         verbatimTextOutput("summary"),
         helpText("The red solid line on the histogram shows a normal distribtion. You should assess whether your data are approximately normally-distributed before proceeding with a t-test"),
         plotOutput("histogram")
         ),
tabPanel("Test Result", h4("Screen output in R"),
         verbatimTextOutput("ttest"),plotOutput("zdist")),
tabPanel("Reproducible Analysis",
         h4("R Script"),
         
         helpText("You will be able to re-run this analysis in R by downloading the R code below"),
         helpText("We recommend RStudio to run the R code and compile a pdf or HTML report that will show the results of your analysis along with the code used"),
         img(src="https://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png"), br(),a("RStudio",href="https://www.rstudio.com/"),br(),
         strong("The input file that you are analysing must be in your R working directory in order for the script to run"),
         helpText("In order to compile the report in RStudio, you will need to install the ggplot2, rmarkdown, reshape2,gridExtra and knitr packages"),br(),
         code("install.packages(c('knitr','ggplot2','rmarkdown,'reshape2','gridExtra'))"),
         br(),
         downloadLink('downloadScript', 'Download R Script'),
         br(),
         br(),
         downloadLink('downloadMarkdown', 'Download R Markdown file')
)
    )
  )
  
  ))