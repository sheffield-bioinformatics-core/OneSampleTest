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
                 'Comma'),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote'),
    textInput("skip", "Number of rows to skip in data file before reading data",value=0),
    textInput("dataCol", "Column containing data to test",value=2),
    
    br(),    
    h2("Analysis Parameters"),
    helpText("Use the histograms and boxplot to judge whether you need to use a parametric, or non-parametric test"),
    checkboxInput("do.parametric",label = "Use Parametric Test?",value = TRUE),
    textInput("mu","True mean",
                value = 0),
    checkboxInput("showMu","Show true mean?", T),
    br(),
    radioButtons("alternative", "Alternative", c("Two-sided"="two.sided", "Greater" = "greater", "Lower"="less"),"two.sided"),
    br(),    
    h2("Analysis Parameters"),
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
         verbatimTextOutput("ttest"),helpText("If you have chosen a Parametric test, the comparison of the calculated test-statistic to the reference distribution will be shown here"),plotOutput("zdist")),
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
         downloadLink('downloadMarkdown', 'Download R Markdown file'),               
         h2("Code preview..."),
         verbatimTextOutput("thecode")
)
    )
  )
  
  ))