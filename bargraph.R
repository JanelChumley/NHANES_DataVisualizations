
require("jsonlite")
require("RCurl")
require("dplyr")
require("plyr")
require("tidyr")
require("ggplot2")
require("ggthemes")
library(shiny)
require(car)
library(plotly)
# Here is the ui script

ui<-shinyUI(fluidPage(
  selectInput(input = "marital",
                     label = "Martial Status: ",
                     c("Divorced", "Living with partner", "Married", "Never married", "Separated", "Widowed"), selected = "Married"),
              
  mainPanel(
    plotlyOutput("barplot"), width = 500, height = 500)
))

#Here is the server script

df<- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="SELECT * FROM DEMOGRAPHICS d INNER JOIN MENTALHEALTH m ON d.id_num = m.id_num INNER JOIN DRUGUSE du ON m.id_num = du.id_num"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_jmc6473', PASS='orcl_jmc6473', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
server <- shinyServer(function(input, output){

  output$barplot<-renderPlotly({

    df <-df %>%
      dplyr::select(MARITAL, ANNUAL_HH_INCOME, DEPRESSED)%>%
      dplyr::filter(MARITAL%in%(input$marital), !ANNUAL_HH_INCOME%in%c("null", "Under $20,000", "$20,000 + "), DEPRESSED!="null" )%>%
      dplyr::group_by(MARITAL, ANNUAL_HH_INCOME, DEPRESSED)%>%
      dplyr::summarize(count = n())%>%dplyr::mutate(percentage = count/sum(count), pos = (cumsum(percentage) - 0.5 * percentage))
    #mapping strings to factors for income brackets
    df$scode<-mapvalues(df$ANNUAL_HH_INCOME, from = c("$0 - 4,999", "$5,000 - 9,999", "$10,000 - 14,999", "$15,000 - 19,999", "$20,000 - 24,999", "$25,000 - 34,999", "$35,000 - 44,999", "$45,000 - 54,999", "$55,000 - 64,999", "$65,000 - 74,999", "$75,000 - 99,999", "$100,000 +"), to=c("1", "2", "3", "4", "5", "6","7", "8", "9","10", "11", "12"))
    
      p<-ggplot(data= df, aes(x =ANNUAL_HH_INCOME, y=percentage, fill=DEPRESSED)) + geom_bar(stat="identity", position ="stack") + labs(title = "") + ggtitle("Depression v. Annual Income") + theme(legend.title =element_text(size=8), plot.title=element_text(size=10, face="bold"), axis.title.x = element_text(size=10), axis.text=element_text(size=7))+ geom_text(data = subset(df,percentage>=0.05), aes(y=pos, label = paste(round(percentage*100,2), "%")), size=2, color="white")+ xlim(c("$0 - 4,999", "$5,000 - 9,999", "$10,000 - 14,999", "$15,000 - 19,999", "$20,000 - 24,999", "$25,000 - 34,999", "$35,000 - 44,999", "$45,000 - 54,999", "$55,000 - 64,999", "$65,000 - 74,999", "$75,000 - 99,999", "$100,000 +")) + xlab("") + ylab("% of Total Count Depressed") +  scale_y_continuous(expand=c(0,0)) + coord_flip() 

ggplotly(p)
})})
shinyApp(ui = ui, server = server)
