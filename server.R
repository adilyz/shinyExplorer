library(shiny)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(shinythemes)
shinyServer(function(input,output)({
  
  filedata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    return(      
      read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    )
  })

  output$MyTable <- renderTable({
    filedata()
  })
  
  data <- reactive({
    if(input$dataset=="MyTable"){
      filedata()  
    }
    else{
      as.data.frame(get(input$dataset))      
    }
  })
  
  ## to output the dataset
  output$dat <- renderPrint({
    data()
  })
  
  output$feat1 <- renderUI({
    selectInput("feature1", "select 1st Feature", choices=names(data()))
  })
  
  output$feat2 <- renderUI({
    selectInput("feature2", "select 2nd Feature", choices=names(data()))
    
  })
  
  output$outc <- renderUI({
    selectInput("outcom", "select outcom", choices=names(data()))
  })
  
  # to output the structure of the dataset
  output$struct <- renderPrint({
    if(input$dataset=="MyTable"){
      str(filedata())  
    }
    else{
      str(get(input$dataset))      
    }
  })
  
  # for summary
  output$summary <- renderPrint({
    if(input$dataset=="MyTable"){
      summary(filedata())  
    }
    else{
      summary(get(input$dataset))      
    }
  })
  
  output$correlation <- renderPrint({
    if(input$dataset=="MyTable"){
      abs(round(cor(filedata(), use = "pair"),2))
    }
    else{
      abs(round(cor(get(input$dataset), use = "pair"),2))
    }
  })
  

  # For plot
  output$plot <- renderPlot({
    data1 <- na.omit(data())
    featur1 <- input$feature1
    featur2 <- input$feature2
    outcom <- input$outcom
    outcomeFactor <- cut2(data1[,outcom],g=4)
    factors <- c("factor","character","ordered")
    numer <- c("numeric","integer")
  

    if ( is.element(class(data1[,featur1]),numer) & is.element(class(data1[,featur2]),numer) & is.element(class(data1[,outcom]),numer)){
      p1 <- ggplot(na.omit(data()), aes_string(x= featur1, y= outcom)) +geom_point() 
      p2 <- ggplot(na.omit(data()), aes_string(x= featur2, y= outcom)) +geom_point()
      p3 <- ggplot(na.omit(data()), aes_string(x= outcomeFactor, y= featur2))+ xlab("Outcome, asFactor") + guides(fill=guide_legend(title="outcome, asFactor")) +geom_boxplot()
      p4 <- ggplot(na.omit(data()), aes_string(x= featur2)) + geom_density(aes_string(group=outcomeFactor,  fill=outcomeFactor), alpha=0.3) +labs(fill="Outcome, asFactor")  
      p5 <- ggplot(na.omit(data()), aes_string(x= featur1, y= featur2, color=outcomeFactor)) + geom_point() + labs(color="Outcome, asFactor") 
      grid.arrange(p1,p2,p3,p4,p5, ncol=3)      
    }
    else if ( is.element(class(data1[,featur1]),numer) &  is.element(class(data1[,featur2]),numer) & is.element(class(data1[,outcom]),factors) ) {
      p1 <- ggplot(na.omit(data()), aes_string(x= featur1, y= featur2, color=data1[,outcom])) +geom_point()
      p2 <- ggplot(na.omit(data()), aes_string(x= featur1)) + geom_density(aes_string(group=data1[,outcom], fill=data1[,outcom]), alpha=0.3) +labs(fill="Outcome, asFactor")
      p3 <- ggplot(na.omit(data()), aes_string(x= outcom, y= featur2)) +geom_boxplot()
      p4 <- ggplot(na.omit(data()), aes_string(x= featur2)) + geom_density(aes_string(group=data1[,outcom], fill=data1[,outcom]), alpha=0.3) +labs(fill="Outcome, asFactor")
      p5 <- ggplot(na.omit(data()), aes_string(x= featur1, y= featur2, color=outcom)) + geom_point() 
      grid.arrange(p1,p2,p3,p4,p5, ncol=3) 
    }
    else if (is.element(class(data1[,featur1]),factors) & is.element(class(data1[,featur2]),numer) & is.element(class(data1[,outcom]),factors) ) {
      p1 <- ggplot(na.omit(data()), aes_string(x= featur1, y= featur2, color=data1[,outcom])) +geom_boxplot()
      p2 <- ggplot(na.omit(data()), aes_string(x= featur1)) + geom_bar(aes_string(group=data1[,outcom], colour=data1[,outcom], fill=data1[,outcom]), alpha=0.3)
      p3 <- ggplot(na.omit(data()), aes_string(x= outcom, y= featur2)) +geom_boxplot()
      p4 <- ggplot(na.omit(data()), aes_string(x= featur2)) + geom_density(aes_string(group=data1[,outcom], colour=data1[,outcom], fill=data1[,outcom]), alpha=0.3)
      grid.arrange(p1,p2,p3,p4, ncol=2) 
    }
    else if ( is.element(class(data1[,featur2]),factors) & is.element(class(data1[,featur1]),numer) & is.element(class(data1[,outcom]),factors) ) {
      p1 <- ggplot(na.omit(data()), aes_string(x= featur2, y= featur1, color=data1[,outcom])) +geom_boxplot()
      p2 <- ggplot(na.omit(data()), aes_string(x= featur2)) + geom_bar(aes_string(group=data1[,outcom], colour=data1[,outcom], fill=data1[,outcom]), alpha=0.3)
      p3 <- ggplot(na.omit(data()), aes_string(x= outcom, y= featur1)) +geom_boxplot()
      p4 <- ggplot(na.omit(data()), aes_string(x= featur1)) + geom_density(aes_string(group=data1[,outcom], colour=data1[,outcom], fill=data1[,outcom]), alpha=0.3)
      grid.arrange(p1,p2,p3,p4, ncol=2) 
    }
    else if ( is.element(class(data1[,featur2]),factors) & is.element(class(data1[,featur1]),numer) & is.element(class(data1[,outcom]),numer) ) {
      p1 <- ggplot(na.omit(data()), aes_string(x= featur2, y= outcom)) +geom_boxplot()
      p2 <- ggplot(na.omit(data()), aes_string(x= outcom)) + geom_density(aes_string(group=data1[,featur2], colour=data1[,featur2], fill=data1[,featur2]), alpha=0.3)
      p3 <- ggplot(na.omit(data()), aes_string(x= featur1, y= outcom)) +geom_point()
      p4 <- ggplot(na.omit(data()), aes_string(x= featur1)) + geom_density(aes_string(group=data1[,featur2], colour=data1[,featur2], fill=data1[,featur2]), alpha=0.3)
      grid.arrange(p1,p2,p3,p4, ncol=2) 
    }
    else if ( is.element(class(data1[,featur1]),factors) & is.element(class(data1[,featur2]),numer) & is.element(class(data1[,outcom]),numer) ) {
      p1 <- ggplot(na.omit(data()), aes_string(x= featur1, y= outcom)) +geom_boxplot()
      p2 <- ggplot(na.omit(data()), aes_string(x= outcom)) + geom_density(aes_string(group=data1[,featur1], colour=data1[,featur1], fill=data1[,featur1]), alpha=0.3)
      p3 <- ggplot(na.omit(data()), aes_string(x= featur2, y= outcom)) +geom_point()
      p4 <- ggplot(na.omit(data()), aes_string(x= featur2)) + geom_density(aes_string(group=data1[,featur1], colour=data1[,featur1], fill=data1[,featur1]), alpha=0.3)
      grid.arrange(p1,p2,p3,p4, ncol=2) 
    }
    else if ( is.element(class(data1[,featur1]),factors) & is.element(class(data1[,featur2]),factors) & is.element(class(data1[,outcom]),numer) ) {
      p1 <- ggplot(na.omit(data()), aes_string(x= featur1, y= outcom)) +geom_boxplot()
      p2 <- ggplot(na.omit(data()), aes_string(x= outcom)) + geom_density(aes_string(group=data1[,featur1], colour=data1[,featur1], fill=data1[,featur1]), alpha=0.3)
      p3 <- ggplot(na.omit(data()), aes_string(x= featur2, y= outcom)) +geom_boxplot()
      p4 <- ggplot(na.omit(data()), aes_string(x= outcom)) + geom_density(aes_string(group=data1[,featur2], colour=data1[,featur2], fill=data1[,featur2]), alpha=0.3)
      grid.arrange(p1,p2,p3,p4, ncol=2) 
    }
    else if ( is.element(class(data1[,featur1]),factors) & is.element(class(data1[,featur2]),factors) & is.element(class(data1[,outcom]),factors) ) {
      p1 <- ggplot(na.omit(data()), aes(x= outcom, group=featur1, colour=featur1, fill=featur1)) + geom_bar(alpha=0.3)
      p2 <- ggplot(na.omit(data()), aes(x= outcom, group=featur2, colour=featur2, fill=featur2)) + geom_bar(alpha=0.3)
      p3 <- ggplot(na.omit(data()), aes(x= featur2, group=featur1, colour=featur1, fill=featur1)) + geom_bar(alpha=0.3)
      p4 <- ggplot(na.omit(data()), aes(x= featur2, group=outcom, colour=outcom, fill=outcom)) + geom_bar(alpha=0.3)
      p5 <- ggplot(na.omit(data()), aes(x= featur1, group=outcom, colour=outcom, fill=outcom)) + geom_bar(alpha=0.3)
      grid.arrange(p1,p2,p3,p4,p5, ncol=3) 
    }
  

    
  }) 
  
}))