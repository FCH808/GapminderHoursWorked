library(shiny)
library(ggplot2)
library(ggthemes)
library(data.table)
library(Hmisc)
library(plyr)
library(reshape2)
library(Hmisc)

hours<- fread("hours_per_week.csv", header = TRUE, drop = 30) #Superfast data.table package (resulting object is both a data.frame and a data.table)
setnames(hours,"Working hours per week", "Country") #setnames for data.tables
hours<- hours[-53,] #Remove the last empty row

hoursLong<- melt(hours, id = c("Country"), value.name = "Hours", variable.name = "Year" )

hoursLong$Decade<- cut2(as.numeric(as.character(hoursLong$Year)), cuts = c(1980, 1990, 2000))
hoursLong$Decade<- factor(hoursLong$Decade, labels = c("1980 - 1989", "1990 - 1999", "2000 - 2007"))

CountryStats<- ddply(hoursLong, .(Country, Decade), summarise,
                     median = round(median(Hours, na.rm = TRUE), 2),
                     mean = round(mean(Hours, na.rm = TRUE), 2),
                     lower = round(quantile(Hours, na.rm = TRUE, probs = 0.25), 2),
                     upper = round(quantile(Hours, na.rm = TRUE, probs = 0.75), 2))

HoursOrdered<- transform(CountryStats, Country = reorder(Country, -mean))

shinyServer(function(input, output) {
  
  passMin<<- reactive({
    as.numeric(input$slider_min)
  })
  
  passMax<<- reactive({
    as.numeric(input$slider_max)
  })
    
  output$mediansPlot <- renderPlot({
    
    yearsStats<<- ddply(hoursLong, .(Year), summarise,
                        median = round(median(Hours, na.rm = TRUE), 2),
                        mean = round(mean(Hours, na.rm = TRUE), 2),
                        lower = round(quantile(Hours, na.rm = TRUE, probs = passMin()), 2),
                        upper = round(quantile(Hours, na.rm = TRUE, probs = passMax()), 2),
                        minC = round(min(Hours, na.rm = TRUE), 2),
                        maxC = round(max(Hours, na.rm = TRUE), 2))    
    
    if(input$theTabs == "summaryTab"){
      output$data_table<- renderTable({
        # If missing input, return to avoid error later in function
        if(is.null(input$country))
          return()
        ## Need to add reactive dataset calls
        dynamicDataset<- subset(hoursLong, Country %in% sort(input$country))
        dynamicDataset<- subset(dynamicDataset, select = -Decade)
        
        wideResults<- dcast(dynamicDataset, Country ~ Year, value.var = "Hours")
      })
      
      output$data_table2<- renderTable({
        
        yearsStats.T<- yearsStats
        setnames(yearsStats, c("mean", "median", "lower", "upper", "minC", "maxC"), 
                 c("Mean of all", "Median", "First Quantile Used", "Second Quantile Used", "Min. Country Mean", "Max Country Mean"))
        yearsStats.T <- melt(yearsStats, id.var="Year", variable.name = "Stats") 
        yearsStats.T<- dcast(yearsStats.T, Stats ~ Year, value.var = "value")
        yearsStats.T
        
      })
    }
    
    if(input$theTabs == "mediansTab"){
      
      dynamicDataset<- subset(hoursLong, Country %in% sort(input$country))
      
      p <- ggplot(yearsStats, aes(x = Year, y = median), 
                  environment = environment()) +
        
        geom_line(data = dynamicDataset,
                  aes(x = Year, y = Hours, color = Country, group = Country)) + 
         
        theme(axis.text.x = element_text(angle = 90))
      
      if(input$quart == 'Yes')
        
        p <- p + geom_errorbar( aes( ymin = yearsStats$lower, ymax = yearsStats$upper), 
                                color = "blue", width = .8) 
      
      if (input$world_text == "Yes")
        p <- p + geom_text(aes(x = Year, y = median, label = median),
                           hjust = -.1, 
                           vjust = 1.2, 
                           size = 3, 
                           color = "DarkBlue") + geom_point(shape = 5) + geom_line(aes(group = Year))
      
      if (input$world_text == "Just points")
        
        p<- p + geom_point(shape = 5, color = "DarkBlue")
      
      if (input$country_text == "Yes")
        p<- p + geom_text(data = dynamicDataset,
                          aes( x = Year,
                               y = Hours,
                               color = Country,
                               group = Country,
                               label = round(Hours, 2)), 
                          hjust = -.1, 
                          vjust = -1.2, 
                          size = 3,
                          show_guide  = FALSE) +
        
        geom_point(data = dynamicDataset,
                   aes(x = Year, y = Hours, color = Country, group = Country))
      
      if (input$country_text == "Just points")
        
        p<- p + geom_point(data = dynamicDataset,
                           aes(x = Year, y = Hours, color = Country, group = Country))
      
      if (input$minmax) 
        
        p <- p + geom_errorbar(aes( ymin = yearsStats$minC, ymax = yearsStats$maxC), 
                               environment = environment(),
                               color = "black", width = 0.4)
      if(input$themes == "Tufte")
        p<- p + theme_tufte()
      if(input$themes == "Economist")
        p<- p + theme_economist()
      if(input$themes == "Solarized")
        p<- p + theme_solarized()
      if(input$themes == "Midnight")
        p<- p + theme_solarized(light = FALSE)
      if(input$themes == "Stata")
        p<- p + theme_stata()
      if(input$themes == "Stephen Few")
        p<- p + theme_few()
      if(input$themes == "Wall Street Journal")
        p<- p + theme_wsj()
      p<- p + theme(axis.text.x = element_text(angle = 90)) + ylab("Mean Hours per Year") 
    }
    
    print(p)
    
  })
  
  output$boxplot <- renderPlot({
    
    if(input$theTabs == "boxplotsTab") {
      
      p<- ggplot(subset(hoursLong, 
                        Country %in% sort(input$country)), 
                 aes(x=Country, y=Hours, fill=Country)) +
        
        geom_boxplot() + 
        
        stat_summary(fun.y = mean, geom="point", shape = 5)
      
      if(input$themes == "Tufte")
        p<- p + theme_tufte()
      if(input$themes == "Economist")
        p<- p + theme_economist()
      if(input$themes == "Solarized")
        p<- p + theme_solarized()
      if(input$themes == "Midnight")
        p<- p + theme_solarized(light = FALSE)
      if(input$themes == "Stata")
        p<- p + theme_stata()
      if(input$themes == "Stephen Few")
        p<- p + theme_few()
      if(input$themes == "Wall Street Journal")
        p<- p + theme_wsj()
      p<- p + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Distribution of Average Hours per Year from 1980 - 2007")
    }
    
    print(p)
    
  })
  
  output$timeseriesPlot <- renderPlot({
    
    if(input$theTabs == "timeSeriesTab"){
      
      dynamicDataset<- subset(hoursLong, Country %in% sort(input$country))
      
      p<- ggplot(dynamicDataset, 
                 aes(x = Year, y = Hours), 
                 environment = environment()) + 
        geom_line(aes(color = Country, group = Country)) + 
        theme(axis.text.x = element_text(angle = 90))
    }
      
    if(input$quart == "blue"){
      
      p <- p + geom_point(data = dynamicDataset,
                          aes(x = Year, y = Hours, color = Country, group = Country),
                          environment = environment() ) +
        
        theme(axis.text.x = element_text(angle = 90))
    }
    
    print(p)
    
  })  
  
  output$decadePlot <- renderPlot({
    
    if(input$theTabs == "decadeTab"){
      
      p<- ggplot(HoursOrdered, aes(x = Country, y = mean, fill = Decade)) + 
        geom_histogram(stat = "identity", color = "black") +
        facet_grid(Decade~.) + 
        guides(fill=FALSE) + 
        ggtitle("Mean Hours Worked per Week by Decade") + 
        xlab("") 

      if(input$themes == "Tufte")
        p<- p + theme_tufte()
      if(input$themes == "Economist")
        p<- p + theme_economist()
      if(input$themes == "Solarized")
        p<- p + theme_solarized()
      if(input$themes == "Midnight")
        p<- p + theme_solarized(light = FALSE)
      if(input$themes == "Stata")
        p<- p + theme_stata()
      if(input$themes == "Stephen Few")
        p<- p + theme_few()
      if(input$themes == "Wall Street Journal")
        p<- p + theme_wsj()
      p<- p + theme(axis.text.x = element_text(angle = 90)) + ylab("Mean Hours Worked")
    }
    
    print(p)
    
  })
  
})



