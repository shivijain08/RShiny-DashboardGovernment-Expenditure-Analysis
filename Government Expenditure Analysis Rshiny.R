#File   : Module 3
#Project: ALY6070: Communication and Visualization for Data Analytics
#Author : Group 1

#
#Clean canvas ----
#clears the variables
#rm(list=ls())
#clears the plots
#dev.off() 
#clears the console
cat("\014") 


# Include the required packages & libraries ----
install.packages('waterfalls')
install.packages('ggrepel')
install.packages('forecast')
install.packages('ggpubr')
install.packages('shinydashboard')
install.packages('shinyDirChoose')
install.packages("dplyr")
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(magrittr) # for Pipe operator(%>%) that forwards the commands to next function
library(dplyr)    # for data manipulation
library(ggplot2)  # for data visualisation
library(plotrix)  # for plotting functions like seting labels, colors, axis etc
library(moments)  # for computing tests like kurtosis, skewness etc
library(sqldf)
library(waterfalls)
library(plyr)
library(ggrepel)
library(forecast)
library(ggpubr)
library(dplyr)
library(stringr)


# Set the file directory ----
setwd("C:/Users/Divsy/Documents/MPS Analytics Courses/Comm_Visualization - ALY6070/Module1")

# Read the file
design <- read.csv("government-expenditure-by-type.csv")

str(design)


design$class <- with(design, ifelse(class =='Direct Development', 'Direct Dev',
                                                    ifelse(class =='Expenditure on Manpower', 'Expense Manpower',
                                                           ifelse(class =='Grants & Capital injections to Organisations','GnC Dev',
                                                                  ifelse(class =='Grants, Subventions & Capital Injections to Organisations', 'GnC Ops',
                                                                         ifelse(class =='International Organisations & Overseas Development Assistance', 'Intl Dev',
                                                                                ifelse(class =='Other Operating Expenditure', 'Other Ops',
                                                                                       ifelse(class =='Social Transfers', 'Social Transfers', 'Transfers to Orgs'))))))))
                                                                                              


summarized_data <- sqldf('select financial_year, category, sum(amount) as amount from design group by financial_year, category')

## Percent Stacked Bar Chart
# Get the levels for type in the required order
summarized_data$category = factor(summarized_data$category, levels = c("Running Cost",  "Transfers", "Development Expenditure"))
summarized_data1 = arrange(summarized_data, financial_year, desc(category))

# Calculate the percentages
summarized_data1 = ddply(summarized_data1, .(financial_year), transform, percent = amount/sum(amount) * 100)

# Format the labels and calculate their positions
summarized_data1 = ddply(summarized_data1, .(financial_year), transform, pos = (cumsum(amount) - 0.5 * amount))
summarized_data1$label = paste0(sprintf("%.0f", summarized_data1$percent), "%")




#Waterfall Chart ----

# summarized_data2 <- sqldf('select class, sum(amount) as amount from design where type = "Operating" and financial_year = "2021" group by class')

summarized_data2 <- sqldf('select class, sum(amount) as amount from design where  financial_year = "2020" group by class')
summarized_data3 <- sqldf('select class, sum(amount) as amount from design where  financial_year = "2021" group by class')
exp_2020 <- c(summarized_data2$amount)
exp_2021 <- c(summarized_data3$amount)
sum(exp_2020)
sum(exp_2021)
 
exp_diff <- exp_2021 - exp_2020
class <- c(summarized_data3$class)
waterfall_data <- data.frame(class,exp_diff )
str(waterfall_data)
start_point <- c('2020', 94056)
waterfall_data <- rbind(start_point,waterfall_data)
waterfall_data$exp_diff <- as.numeric(waterfall_data$exp_diff)




          
# ARIMA Forecasting ----
df1 <- aggregate(design$amount, list(design$financial_year, design$type), FUN=sum)
names(df1)[1]="financial_year"
names(df1)[2]="type"
names(df1)[3]="amount"
df1
df2 <- filter(df1, type == "Operating")
df3 <- filter(df1, type == "Development")
tsdata <- ts(df2$amount, frequency = 1, start =c (1997,1) )
tsdata1 <- ts(df3$amount, frequency = 1, start =c (1997,1) )
autoarima1 <- auto.arima(tsdata)
forecast1<- forecast(autoarima1,h=5)
forecast1
autoarima2 <- auto.arima(tsdata1)
forecast2<- forecast(autoarima2,h=5)
forecast2


#Type Dashboard ####
total_amount_by_type <- sqldf('select financial_year, type, sum(amount) as total_amount from design  group by financial_year, type')

# total_amount_by_type <- as.data.frame(total_amount_by_type)

total_amount_dev = filter(total_amount_by_type , total_amount_by_type$type == 'Development' )
total_amount_dev$total_amount_cumsum = cumsum(total_amount_dev$total_amount)
total_amount_opr = filter(total_amount_by_type , total_amount_by_type$type == 'Operating' )
total_amount_opr$total_amount_cumsum = cumsum(total_amount_opr$total_amount)

data_for_area <- rbind(total_amount_opr, total_amount_dev)

data_for_area$type <- as.character(data_for_area$type)
data_for_area$type <- factor(data_for_area$type,
                             levels = c("Operating", "Development"),
                             labels = c("Operating", "Development"))




#################################### R Shiny ###################################

##UI####
ui <- dashboardPage(
  
  
  dashboardHeader(title="Government Expenses", titleWidth = 400),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Intro", tabName = "intro"),
                menuItem("Expenses by Type", tabName = "type"),
                menuItem("Expenses by Category", tabName = "category"),
                conditionalPanel("input.sidebar == 'category'", sliderInput("range",label = "Choose a start and end year:",min = min(design$financial_year), max = max(design$financial_year),
                                                                        value = c(1997, 2021),sep = "",round = TRUE,step = 1)),
                
                menuItem("Estimated Expenses for 2021", tabName = "waterfall"),
                menuItem("Expense Forecast", tabName = "forecast")
    )
  ),
  
  dashboardBody(
    tabItems(
      #####Intro tab####
      tabItem(tabName = 'intro',
              fluidRow(column(width = 12,
                              tags$img(src="Intro.png"),
                              align = "center")
              )),
      #####Expenses by Type ####
      tabItem(tabName = 'type',
              fluidRow(
                box(height = 450, plotOutput("piechart",height = 390),
                    title = "Percenatge of Expenditure by Type",
                    status = "primary",
                    width = 4,
                    solidHeader = TRUE),
                box(height = 450, plotOutput("area",height = 390),
                    title = "Cummulative Area Chart",
                    status = "primary",
                    width = 8,
                    solidHeader = TRUE)
              ),
              fluidRow(
                box(height = 450, plotOutput("linetype",height = 390),
                    title = "Estimated Expenses for 2021",
                    status = "primary",
                    width = 12,
                    solidHeader = TRUE)
                
              )
      ),
      #####Expenses by Class ####
      tabItem(tabName = 'category',
              fluidRow(
                column(4,box(plotOutput("bargraph",height = "870px", width="500px"),status="primary",width="100px",solidHeader = TRUE,title="Trend Chart",height = "922px")),
                column(4,box(plotOutput("doughnut1",height = "375px"),width="50px",status="primary",solidHeader = TRUE,title="Operating Class Ratio",height = "450px")),
                column(4,box(plotOutput("doughnut2",height = "375px"),width="50px",status="primary",solidHeader = TRUE,title="Development Class Ratio",height = "450px")),
                column(4,box(plotOutput("linechart_op",height = "375px"),width="50px",status="primary",solidHeader = TRUE,title="Operating Class Trend",height = "450px")),
                column(4,box(plotOutput("linechart_dev",height = "375px"),width="50px",status="primary",solidHeader = TRUE,title="Development Class Trend",height = "450px")))
      ),
      
      #####Estimated expenses for 2021####
      tabItem(tabName = 'waterfall',
              fluidRow(
                box(height = 900, align = 'right', plotOutput("waterfall_graph",height = 800),
                title = "Estimated Expenses for 2021",
                status = "primary",
                width = 12,
                solidHeader = TRUE)
              )
      ),
      #####Forecast####
      tabItem(tabName = 'forecast',
              fluidRow(
                box(height = 900, align = 'centre', plotOutput("arima",height = 800),dataTableOutput('table'),
                    title = "Forecast of Expenses",
                    status = "primary",
                    width = 12,
                    solidHeader = TRUE)
              )
      )
      
      )))

##SERVER####
server <- function(input, output, session){
  ##Expenses by Type####
  output$piechart <-renderPlot({
   
    types <- table(design$type)
    
    total <- types[1]+types[2]
    
    proportion_by_type <- c((types[1]/total)*100,(types[2]/total)*100)
    proportion_by_type
    
    #Pie chart to display the proportion split across expenditure type
    par(mar=c(1,3,3,3))
    
    pie(proportion_by_type,
        labels = paste(print(format(round(proportion_by_type, 0), nsmall = 0)),"%"),
        col = c("#EFB366","#287271" )
    )
    
    pielegends <- c("Development","Operating")
    
    legend("topleft", inset = -0.05, cex = 1.6,
           legend = (pielegends), 
           text.col = c("#EFB366","#287271"), 
           bty = "n")
  })
  
  output$area <-renderPlot({
    
    
    ggplot() + geom_area(aes(y = total_amount_cumsum , x = financial_year, fill = type), 
                         data = data_for_area) +
      theme( axis.text.y=element_blank(),  #remove y axis labels
             axis.ticks.y=element_blank(),  #remove y axis ticks
             panel.background = element_rect(fill='transparent')
      ) + labs(x = "", y = "" ) +  theme(axis.text.x = element_text(size = 15)) +
      scale_fill_manual(values = c("#EFB366","#287271" )) + theme(legend.position = "none") + 
      annotate("text", label = "A major chunk of the government’s expenditure throughout these years was \n around operating costs as it covers most of the area in the plot.",
                  x = 2003 , size = 5, y =800000,color="black",family = 'Times New Roman') + 
      annotate("text", label = "$891K", x = 2022 , size = 5, y =1180000,color="#EFB366",family = 'Times New Roman') +    
      annotate("text", label = "$307K",x = 2022 , size = 5, y =307000,color="#287271",family = 'Times New Roman')
  })
  
  output$linetype <-renderPlot({
    ggplot(data = total_amount_by_type, aes(x=financial_year, y=total_amount)) + 
      theme_classic()+geom_line(aes(col=type),size=1.3)  + labs(x = "", y = "" ) + 
      scale_color_manual(values = c("#EFB366","#287271" ))+ theme(legend.position = "none") + theme(axis.text.y = element_text(size = 15)) + theme(axis.text.x = element_text(size = 15)) +
      annotate('text',label = "Operating Expense has increased more significantly than Development Expense over the years. \n The operating costs are increasing rapidly with time and by the end of 2021, it is around $82K \n  whereas the rise in the development costs are not much if compared it is just $20K at the end of 2021", x = 2003 , y = 65000, size = 5, color="black",family = 'Times New Roman')+ 
      annotate("text", label = "$82K", x = 2021 , size = 5, y =83500,color="#287271",family = 'Times New Roman') +    
      annotate("text", label = "$20K",x = 2021, size = 5, y =21100,color="#EFB366",family = 'Times New Roman')
      
  })
  
  ##Expenses Class ####
  output$doughnut1 <-renderPlot({
    
    # # shinyDirchoose (input, 'dir', roots = design, session = session)
    # r1 <- input$range
    df5 <- design[design$financial_year >= input$range[1] & design$financial_year <= input$range[2],]
    df5 <- aggregate(df5$amount, list(df5$type, df5$class), FUN=sum)
    names(df5)[1]="type"
    names(df5)[2]="class"
    names(df5)[3]="amount"
    # cat(file=stderr(), paste0("my print statement: ",df5, "\n"))
    df6 <- filter(df5, type == "Operating")
    df6$newlabel <- ifelse(df6$class == "Grants, Subventions & Capital Injections to Organisations", "Grants, Sub., Capital Injections to Org.",
                           ifelse(df6$class == "International Organisations & Overseas Development Assistance","Inter. Org. & Overseas Dev. Assistance",df6$class))
    df6$fraction <- df6$amount / sum(df6$amount)
    
    # Compute the cumulative percentages (top of each rectangle)
    df6$ymax <- cumsum(df6$fraction)
    
    # Compute the bottom of each rectangle
    df6$ymin <- c(0, head(df6$ymax, n=-1))
    
    # Compute label position
    df6$labelPosition <- (df6$ymax + df6$ymin)/2
    
    # Compute a good label
    df6$label <- paste0(round(df6$fraction*100,2), "%")
    
    
    df6 <- df6[order(df6$fraction, decreasing = TRUE), ]
    
    
    ggdonutchart(df6, "fraction",label="newlabel",
                 fill="class",lab.font = c(5, "plain", "black"),
                 palette = c("#8AB17D","#2A9D8F","#E76F51","#264653","#F4A261","#5AA786"), color = "white")
      })
  
  output$doughnut2 <-renderPlot({
    df5 <- design[design$financial_year >= input$range[1] & design$financial_year <= input$range[2],]
    df5 <- aggregate(df5$amount, list(df5$type, df5$class), FUN=sum)
    names(df5)[1]="type"
    names(df5)[2]="class"
    names(df5)[3]="amount"
    # cat(file=stderr(), paste0("my print statement: ",df5, "\n"))
    
    df7 <- filter(df5, type == "Development")
    df7$fraction <- df7$amount / sum(df7$amount)
    # Compute the cumulative percentages (top of each rectangle)
    df7$ymax <- cumsum(df7$fraction)
    
    # Compute the bottom of each rectangle
    df7$ymin <- c(0, head(df7$ymax, n=-1))
    
    # Compute label position
    df7$labelPosition <- (df7$ymax + df7$ymin)/2
    
    # Compute a good label
    df7$label <- paste0(round(df7$fraction*100,2), "%")
    ggdonutchart(df7, "fraction",label="class",
                 fill = "class", color = "white",lab.font = c(5, "plain", "black"),
                 palette = c("#EFB366","#287271"))
    
                })
  
  output$bargraph <-renderPlot({
    
    df5 <- design[design$financial_year >= input$range[1] & design$financial_year <= input$range[2],]
    df5 <- aggregate(df5$amount, list(df5$type, df5$class), FUN=sum)
    names(df5)[1]="type"
    names(df5)[2]="class"
    names(df5)[3]="amount"
    # cat(file=stderr(), paste0("my print statement: ",df5, "\n"))
    
    df5 <- df5[order(df5$amount, decreasing = TRUE), ]
    df5<- df5 %>% mutate(
      ## set justification based on data 
      ## so that only the first label is placed inside
      place = if_else(row_number() == 1, 1, 0),
      col = if_else(row_number() == 1, "#FFFFFF","#000000")
      
    )
   
    ggplot(df5, aes(x = amount,
                    y = reorder(class,amount)) )+ 
      geom_bar(stat = "identity",fill=c("#264653","#287271","#2A9D8F","#5AA786","#8AB17D","#EFB366","#F4A261","#E76F51"))+
      theme(legend.position="none")+
      geom_text(
        aes(label = amount, hjust = place), 
        size = 4.5, fontface = "bold",color= df5$col
      ) +
      theme_classic() +
      labs(
        x="", y = "")+
      theme(plot.title = element_text(hjust = 0.5,size = 18,face="bold"))+
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
      scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) + theme(axis.text.y = element_text(size = 15))
    
  })
  
  output$linechart_op <-renderPlot({
    df5 <- design[design$financial_year >= input$range[1] & design$financial_year <= input$range[2],]
    
    # cat(file=stderr(), paste0("my print statement: ",df5, "\n"))
    l1 <- aggregate(df5$amount, list(df5$financial_year,df5$type,df5$class), FUN=sum)
    
    names(l1)[1]="year"
    names(l1)[2]="type"
    names(l1)[3]="class"
    names(l1)[4]="amount"
    df8 <- filter(l1, type == "Operating")
    
    
    ggplot(df8, aes(x=year, y=amount)) + 
      geom_line(aes(color=class),size=1.3) +
      theme_classic() +
      theme(legend.position = "none")+
      labs(
        x="", y = "")+
      theme(plot.title = element_text(hjust = 0.5,size = 18,face="bold"))+
      scale_color_manual(values = c("#8AB17D","#2A9D8F","#E76F51","#264653","#F4A261","#5AA786"))+ theme(axis.text.y = element_text(size = 15)) + theme(axis.text.x = element_text(size = 15)) 
    
    
  })
  
  output$linechart_dev <-renderPlot({
    df5 <- design[design$financial_year >= input$range[1] & design$financial_year <= input$range[2],]
    l2 <- aggregate(df5$amount, list(df5$financial_year,df5$type,df5$class), FUN=sum)
    
    names(l2)[1]="year"
    names(l2)[2]="type"
    names(l2)[3]="class"
    names(l2)[4]="amount"
    df9 <- filter(l2, type == "Development")
    ggplot(df9, aes(x=year, y=amount)) + 
      geom_line(aes(color=class),size=1.3) +
      theme_classic() +
      theme(legend.position = "none")+
      labs( 
        x="", y = "")+
      theme(plot.title = element_text(hjust = 0.5,size = 18,face="bold"))+
      scale_color_manual(values = c("#EFB366","#287271")) + theme(axis.text.y = element_text(size = 15)) + theme(axis.text.x = element_text(size = 15))
    
    
  })
  
  
  
  ##Estimated Forecast for 2021  ####
  output$waterfall_graph <- renderPlot({
  
    waterfall(.data = waterfall_data, values = waterfall_data$exp_diff, linetype = 1, 
              calc_total = TRUE,
              total_axis_text = "2021",
              rect_text_size = 1.5,
              total_rect_color = "#2A9D8F",
              total_rect_text_color = "black",
              fill_by_sign = FALSE,
              fill_colours = c('#2A9D8F','#EFB366','#EFB366','#EFB366','#EFB366','#264653','#264653','#264653','#EFB366'),
              put_rect_text_outside_when_value_below = 1 * (max(cumsum(waterfall_data$exp_diff))-min(cumsum(waterfall_data$exp_diff))))+
      theme_classic()+ labs(x = "", y = "" )  + theme(axis.text.y = element_text(size = 15)) + theme(axis.text.x = element_text(size = 15)) +
      annotate("text", label = "The expenses for 2021 is estimated to be 102K which is 8k more than the expenses of FY2020. \n The highest increase in expense is estimated to be in Transfers to orgs followed by Direct Dev and FnC Ops. \n The highest decrease in expense is estimated to be in Social Transfers followed by Other ops and Intl Dev.",
               x = 'GnC Dev' , size = 7, y =60000,color="black",family = 'Times New Roman') 
      
  })
  
  
  ##Expenses Forecast####
  output$arima <- renderPlot({
    ggplot(data=df2,aes(x = financial_year,y=amount)) +
      geom_line(aes(y = amount), color = "#287271",size=1.3) +
      geom_line(aes(y = df3$amount), color = "#EFB366",size=1.3) +
      autolayer(forecast1, color="#287271")+
      autolayer(forecast2, color="#EFB366")+
      theme_classic() + theme(axis.text.y = element_text(size = 15)) + theme(axis.text.x = element_text(size = 15)) +
      labs(
        x="", y = "")+
      theme(legend.position = "none",axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) + 
      annotate("text", label = "The operating cost forecast shows a positive upward trend while the development cost forecast seems to be constant over the upcoming years. \n This clearly implies that the government will invest more in operating costs in the upcoming years whereas \n the expenditure by the government for the Development costs will still be somewhat constant in future.",
               x = 2008 , size = 6, y =80000,color="black",family = 'Times New Roman')+ 
      annotate("text", label = "Operating",
               x = 1999 , size = 8, y =120000,color="#287271",family = 'Times New Roman')+ 
      annotate("text", label = " Development",
               x = 1999 , size = 8, y =115000,color="#EFB366",family = 'Times New Roman')+ 
      annotate("text", label = "$112K",
               x = 2027 , size = 7, y =115000,color="#287271",family = 'Times New Roman')+ 
      annotate("text", label = "$19K",
               x = 2027 , size = 7, y =20000,color="#EFB366",family = 'Times New Roman')
    
  })
}


shinyApp(ui, server)



##References: 
## https://r-charts.com/flow/waterfall-chart/
## https://r-coder.com/barplot-r/
## https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
## https://stackoverflow.com/questions/22231124/how-to-draw-stacked-bars-in-ggplot2-that-show-percentages-based-on-group


