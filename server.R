library(shiny)
library(ggplot2)
library(reshape2)
library(gridExtra)


shinyServer(function(input, output){
  
  data <- reactive({inFile <- input$file1
  
                    if (is.null(inFile))
                    return(NULL)
                    print(inFile$datapath)
                    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
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
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")
  
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
  
  
  
}
)