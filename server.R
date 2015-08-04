library(shiny)
library(ggplot2)
library(reshape2)
library(gridExtra)


shinyServer(function(input, output){
  
  data <- reactive({inFile <- input$file1
  
                    if (is.null(inFile))
                    return(structure(list(Month = structure(c(5L, 4L, 8L, 1L, 9L, 7L, 6L, 
                                                              2L, 12L, 11L, 10L, 3L), .Label = c("April", "August", "December", 
                                                                                                 "February", "January", "July", "June", "March", "May", "November", 
                                                                                                 "October", "Sept"), class = "factor"), Failure = c(2.9, 2.99, 
                                                                                                                                                    2.48, 1.48, 2.71, 4.17, 3.74, 3.04, 1.23, 2.72, 3.23, 3.4)), .Names = c("Month", 
                                                                                                                                                                                                                            "Failure"), class = "data.frame", row.names = c(NA, -12L)))
  
  
                    print(inFile$datapath)
                    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote,skip=input$skip)
                    #read.csv("GraphPad Course Data/diseaseX.csv")
  })
  
#  output$plot <- renderPlot({
#    plot(data(), xlab="X", ylab="Y", ylim=c(-300,800))
#    if(input$line) {
#      abline(lm(Y ~ X, data=data()), col="dark blue")
#    }
#    if(input$means) {
#      abline(v = mean(data()[,1]), lty="dotted")
#      abline(h = mean(data()[,2]), lty="dotted")
#    } 
#    if(input$ant) {
#      model = lm(Y ~ X, data=data())
#      txt = paste("The equation of the line is:\nY = ",
#                  round(coefficients(model)[1],0)," + ",
#                  round(coefficients(model)[2],3),"X + error")
      
#      boxed.labels(50,600,labels=txt,bg="white", cex=1.25)
#    }    
    
#  })
 #
  
  output$mytable= renderDataTable({
    df <- data()
    df
    dput(df, file="data.rda")
  }
  )
  

  
  output$histogram<- reactivePlot(function(){
    
  df <- data()
  datacol <- as.numeric(input$dataCol)
  
  mu <- as.numeric(input$mu)
                   
  if(input$showMu){ xlim <-c(min(mu-5, min(df[,datacol])), max(mu+5, max(df[,datacol])))
  } else xlim <- c(min(df[,datacol]), max(df[,datacol]))
  
   
  colnames(df)[datacol] <- "X"
  
  p<- ggplot(df, aes(x=X)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") + ylab("")

  p <- p + stat_function(fun=dnorm,
                           color="red",
                           arg=list(mean=mean(df$X), 
                                    sd=sd(df$X)))
  
  p
  
  if(input$showMu) p <- p + geom_vline(xintercept = mu,lty=2,col="red")
  print(p)

  }
  )
  
  output$boxplot<- reactivePlot(function(){
    
    df <- data()
    mu <- as.numeric(input$mu)
    datacol <- as.numeric(input$dataCol)
    
    if(input$showMu) {
      xlim <-c(min(mu-5, min(df[,datacol])), max(mu+5, max(df[,datacol])))
    } else xlim <- c(min(df[,datacol]), max(df[,datacol]))
    
    boxplot(df[,datacol],xlab=colnames(df)[datacol],ylim=xlim,horizontal=TRUE)
    
    if(input$showMu) abline(v = mu,lty=2,col="red")
    
  }
  )
  

  
  output$ttest <-renderPrint({
    df <- data()
    datacol <- as.numeric(input$dataCol)
    X <- df[,datacol]
    alternative = input$alternative
    mu <- as.numeric(input$mu)
    t.test(X,mu=mu,alternative=alternative)
  })

  

  output$summary <- renderPrint({
    datacol <- as.numeric(input$dataCol)
    
    summary(data()[,datacol])
  })
  




  output$zdist <- reactivePlot(function(){
  
    mu <- as.numeric(input$mu)
    alternative = input$alternative
    mu <- as.numeric(input$mu)
   
    df <- data()
    datacol <- as.numeric(input$dataCol)
    degfree <- nrow(df)-1
    X <- df[,datacol]
    tstat <- t.test(X,mu=mu,alternative=alternative)$statistic
    

    alternative = input$alternative
    
    df <- data.frame(ts = rt(10000,df=degfree))

    
    p<- ggplot(df, aes(x=ts)) + 
      geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                     binwidth=.5,
                     colour="black", fill="white") +
      geom_density()
    
    xlim <- c(min(tstat-0.2,min(df$ts)), max(tstat+0.2, max(df$ts)))
    
    critvals <- c(qt(0.05, degfree),qt(0.95,degfree))
    rect1 <- data.frame(xmin = min(critvals[1],xlim),xmax = critvals[1], ymin=-Inf,ymax=Inf)
    rect2 <- data.frame(xmin = critvals[2],xmax = max(critvals[2],xlim), ymin=-Inf,ymax=Inf)
    
   p <- switch(alternative,
    "two.sided" = p + geom_rect(data=rect1,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey20", alpha=0.5, inherit.aes = FALSE) + geom_rect(data=rect2,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey20", alpha=0.5, inherit.aes = FALSE),
    "greater" = p + geom_rect(data=rect2,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey20", alpha=0.5, inherit.aes = FALSE),
    "less" =  p + geom_rect(data=rect1,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey20", alpha=0.5, inherit.aes = FALSE)
   )   
    p <- p + geom_vline(xintercept = tstat,lty=2,col="red") + xlim(xlim)
    print(p)
  })

output$downloadScript <- downloadHandler(
  filename = function() {
    paste(input$outfile, '.R', sep='')
  },
  content = function(file) {
    #cat(file=file,as.name('myfile <- file.choose()\n'))
    #cat(file=file,as.name(paste0('sep <- \'', input$sep,'\'','\n')),append=TRUE)
    #cat(file=file,as.name(paste0('quote <- \'', input$quote,'\'','\n')),append=TRUE)
    #cat(file=file,as.name(paste('header <- ', input$header,'\n')),append=TRUE)
    #cat(file=file,as.name(paste('skip <- ', input$skip,'\n')),append=TRUE)
    #cat(file=file,as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)\n"),append=TRUE)
    df <- dget("data.rda")
    
    cat(file=file,as.name("data <-"))
    cat(file=file,capture.output(dput(df)),append=TRUE)
    cat(file=file,"\n",append=TRUE)
    cat(file=file,as.name("head(data)\n"),append=TRUE)
  
    cat(file=file,as.name(paste("datacol <- ", input$dataCol,'\n')),append=TRUE)
    cat(file=file,as.name("X <- data[,datacol]\n"),append=TRUE)
    cat(file=file,as.name("summary(X)\n"),append=TRUE)
    cat(file=file,as.name("boxplot(X,horizontal=TRUE)\n"),append=TRUE)
    
    cat(file=file,as.name("colnames(data)[datacol] <- 'X'\n"),append=TRUE)
    cat(file=file, as.name("library(ggplot2)\n"),append=TRUE)
    cat(file=file, as.name("ggplot(data, aes(x=X)) + geom_histogram(aes(y=..density..),binwidth=.5,colour='black', fill='white')+ stat_function(fun=dnorm,color='red',arg=list(mean=mean(data$X), sd=sd(data$X)))\n"),append=TRUE)
    
    cat(file=file,as.name(paste0('alternative <- \'', input$alternative,'\'','\n')),append=TRUE)
    cat(file=file,as.name(paste("mu <- ", input$mu,'\n')),append=TRUE)
    cat(file=file,as.name("t.test(X,mu=mu,alternative=alternative)\n"),append=TRUE)
    cat(file=file,as.name("sessionInfo()\n"),append=TRUE)
    #formatR::tidy_source(source=file,output = file)
  }
)


output$downloadMarkdown <- downloadHandler(
  filename = function() {
    paste(input$outfile, '.Rmd', sep='')
  },
  content = function(file) {
    #cat(file=file,as.name('myfile <- file.choose()\n'))
    #cat(file=file,as.name(paste0('sep <- \'', input$sep,'\'','\n')),append=TRUE)
    #cat(file=file,as.name(paste0('quote <- \'', input$quote,'\'','\n')),append=TRUE)
    #cat(file=file,as.name(paste('header <- ', input$header,'\n')),append=TRUE)
    #cat(file=file,as.name(paste('skip <- ', input$skip,'\n')),append=TRUE)
    #cat(file=file,as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)\n"),append=TRUE)
    script <- gsub(".Rmd",".R",file)
    df <- dget("data.rda")
    
    cat(file=script,as.name("data <-"))
    cat(file=script,capture.output(dput(df)),append=TRUE)
    cat(file=script,"\n",append=TRUE)
    cat(file=script,as.name("head(data)\n"),append=TRUE)
    
    cat(file=script,as.name(paste("datacol <- ", input$dataCol,'\n')),append=TRUE)
    cat(file=script,as.name("X <- data[,datacol]\n"),append=TRUE)
    cat(file=script,as.name("summary(X)\n"),append=TRUE)
    cat(file=script,as.name("boxplot(X,horizontal=TRUE)\n"),append=TRUE)
    cat(file=script,as.name("colnames(data)[datacol] <- 'X'\n"),append=TRUE)
    cat(file=script, as.name("library(ggplot2)\n"),append=TRUE)
    cat(file=script, as.name("ggplot(data, aes(x=X)) + geom_histogram(aes(y=..density..),binwidth=.5,colour='black', fill='white')+ stat_function(fun=dnorm,color='red',arg=list(mean=mean(data$X), sd=sd(data$X)))\n"),append=TRUE)
    
    cat(file=script,as.name(paste0('alternative <- \'', input$alternative,'\'','\n')),append=TRUE)
    cat(file=script,as.name(paste("mu <- ", input$mu,'\n')),append=TRUE)
    cat(file=script,as.name("t.test(X,mu=mu,alternative=alternative)\n"),append=TRUE)
    cat(file=script,as.name("sessionInfo()\n"),append=TRUE)
    knitr:::spin(hair=script,knit = FALSE)
    rmd <- readLines(file)
    
    cat(file = file, paste(input$title, "\n=======================\n"))
    cat(file=file, as.name(paste("###", input$name, "\n")),append=TRUE)    
    cat(file=file, as.name(paste("### Report Generated at: ", as.character(Sys.time()), "\n")),append=TRUE)    
    
    for(i in 1:length(rmd)){
      cat(file=file, as.name(paste(rmd[i], "\n")),append=TRUE)
      
    }
    
    #    formatR::tidy_urce(file,output = file)
  }
)


output$downloadPDF <- downloadHandler(
  filename = function(input) {
    paste('mycode-', Sys.Date(), '.html', sep='')
  },
  content = function(file) {
    #cat(file=file,as.name('myfile <- file.choose()\n'))
    #cat(file=file,as.name(paste0('sep <- \'', input$sep,'\'','\n')),append=TRUE)
    #cat(file=file,as.name(paste0('quote <- \'', input$quote,'\'','\n')),append=TRUE)
    #cat(file=file,as.name(paste('header <- ', input$header,'\n')),append=TRUE)
    #cat(file=file,as.name(paste('skip <- ', input$skip,'\n')),append=TRUE)
    #cat(file=file,as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)\n"),append=TRUE)
    script <- gsub(".html",".R",file)
    df <- dget("data.rda")
    
    cat(file=script,as.name("data <-"))
    cat(file=script,capture.output(dput(df)),append=TRUE)
    cat(file=script,"\n",append=TRUE)
    cat(file=script,as.name("head(data)\n"),append=TRUE)
    
    cat(file=script,as.name(paste("datacol <- ", input$dataCol,'\n')),append=TRUE)
    cat(file=script,as.name("X <- data[,datacol]\n"),append=TRUE)
    cat(file=script,as.name("summary(X)\n"),append=TRUE)
    cat(file=script,as.name("boxplot(X,horizontal=TRUE)\n"),append=TRUE)
    cat(file=script,as.name("hist(X)\n"),append=TRUE)
    cat(file=script,as.name(paste0('alternative <- \'', input$alternative,'\'','\n')),append=TRUE)
    cat(file=script,as.name(paste("mu <- ", input$mu,'\n')),append=TRUE)
    cat(file=script,as.name("t.test(X,mu=mu,alternative=alternative)\n"),append=TRUE)
    cat(file=script,as.name("sessionInfo()\n"),append=TRUE)
    o <- knitr:::spin(hair=script,knit = FALSE)
    knitr:::knit2html(o)
    #    formatR::tidy_urce(file,output = file)
  }
)

output$thecode <- renderPrint({
  
  print(as.name(paste('myfile <- file.choose()')))
  print(as.name(paste0('sep <- \'', input$sep,'\'')))
  print(as.name(paste0('quote <- \'', input$quote,'\'')))
  print(as.name(paste('header <- ', input$header)))
  print(as.name(paste('skip <- ', input$skip)))
  print(as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)"))
  
  #dump <- dput(data)
  #print(as.name(paste("data <-", capture.output(dput(data)))))
  print(as.name("head(data)"))
  
  print(as.name(paste("datacol <- ", input$dataCol)))
  print(as.name("X <- data[,datacol]"))
  print(as.name("summary(X)"))
  print(as.name("boxplot(X,horizontal=TRUE)"))
  
  print(as.name("colnames(data)[datacol] <- 'X'"))
  print(as.name("library(ggplot2)"))
  print(as.name("ggplot(data, aes(x=X)) + geom_histogram(aes(y=..density..),binwidth=.5,colour='black', fill='white')+ stat_function(fun=dnorm,color='red',arg=list(mean=mean(data$X), sd=sd(data$X)))"))
  
  
  print(as.name(paste0('alternative <- \'', input$alternative,'\'')))
  print(as.name(paste("mu <- ", input$mu)))
  print(as.name("t.test(X,mu=mu,alternative=alternative)"))
  print(as.name("sessionInfo()"))
}
)
  
  
}
)