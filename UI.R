####shiny::runGitHub("OneSidedT","markdunning")

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("T-test Example"),
  
  sidebarPanel(
    h2("Data Import Parameters"),
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 'Comma'),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote'),
    textInput("skip", "Number of rows to skip in data file before reading data",value=0),

    
    br(),    
    h2("Analysis Parameters"),
    textInput("dataCol", "Column containing data to test",value=2),
    textInput("mu","True mean",
                value = 0),
    checkboxInput("showMu","Show true mean?", T),
    br(),
    radioButtons("alternative", "Alternative", c("Two-sided"="two.sided", "Greater" = "greater", "Lower"="less"),"two.sided"),
    #submitButton ('Generate R Code', icon('toggle-right'))
    textInput("outfile", "What to call the output R script",value="analysis"),
    textInput("name", "Your Name",value="Anon."),
    textInput("title", "What title to use in the report",value="My R Analysis")
    ),
  
  mainPanel(
    tabsetPanel(
#      tabPanel("Plot",plotOutput("plot")),
      tabPanel("The data", dataTableOutput("mytable")),
#      tabPanel("Boxplot",plotOutput("boxplot")),
#      tabPanel("Histogram",plotOutput("histogram")),

#      tabPanel("Summary Statistics",
#               h4("Screen output in R"),
#               verbatimTextOutput("summary")),
tabPanel("Data Distribution", helpText("The boxplot and histogram of the data are shown below"),
         plotOutput("boxplot"),
         verbatimTextOutput("summary"),
         helpText("The red solid line on the histogram shows a normal distribtion. You should assess whether your data are approximately normally-distributed before proceeding with a t-test"),
         plotOutput("histogram")
         ),
tabPanel("t test", h4("Screen output in R"),
         plotOutput("zdist"),
         verbatimTextOutput("ttest")),
      tabPanel("R code",
               helpText("You will be able to re-run this analysis in R by downloading the R code below"),
               h4("Code Preview"),
               verbatimTextOutput("thecode"),
               downloadLink('downloadScript', 'Download R Script'),
               br(),
               downloadLink('downloadMarkdown', 'Download R Markdown'),
               br(),
  #             downloadLink('downloadPDF', 'Download HTML Report')
  helpText("We recommend RStudio to run the R code and compile reports"),
  img(src="https://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png"), br(),a("RStudio",href="https://www.rstudio.com/"),br(),
  helpText("In order to compile the report in RStudio, you will need to install the ggplot2, rmarkdown and knitr packages"),br(),
  code("install.packages(c('knitr','ggplot2','rmarkdown'))")
              )
    )
  )
  
  ))