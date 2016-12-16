# NHANES_DataVisualizations

##**Data Source**
The data comes from the National Health and Nutrition Examination Survey (NHANES), from the 2013-14 session. NHANES is designed to assess the health and nutritional status of adults and children in the U.S., and is a major program of the National Center for Health Statistics (NCHS). The survey examines a nationally representative sample of about 5,000 people each year; they are in counties across the country, 15 of which are visited each year. We downloaded six tables of data: responses to questions about alcohol use, demographics, drug use, income, mental health, and occupation. The dataset can be found [here](https://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx?Component=Demographics&CycleBeginYear=2013). 

Please click on the following link to view the Flexdashboard with the dynamic data visualization application. 
[Flex Dashboard Link](https://janelchumley.shinyapps.io/FlexDashboard/)

##**Flexdashboard Storyboard Creation**
**Creating the markdown file**
Step 1: Create an R markdown file with the flexdashboard::flex_dashboard output format. 

Step 2: Add the storyboard: true option to the dashboard and runtime: shiny.

Step 3: Include a set of level 3 (###) dashboard components. Each component will be allocated it’s own frame in the storyboard, with the section title used as the navigation caption.

Step 4: Call all required R libraries
```{r setup, include=FALSE}
library(flexdashboard)
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
require(rbokeh)
```
###**Tab 1: Bar Graph Using Shiny and Plotly HTML Widget**
-----------------------------------------------------------------------
### Bar Graph: Depression v. Annual Income by Marital Status
```{r, echo=FALSE}
#Here is the server script
selectInput(input = "marital",
                     label = "Select a Martial Status: ",
                     c("Divorced", "Living with partner", "Married", "Never married", "Separated", "Widowed"), selected = "Married")
plotlyOutput("barplot",width = "auto", height = "auto")
  output$barplot<-renderPlotly({
df1<- read.csv("mentalHealth.reformatted.csv", stringsAsFactors = FALSE)
df2<- read.csv("demographics.reformatted.csv", stringsAsFactors = FALSE)
df<-merge(df1, df2)
    df <-df %>%
      dplyr::select(MARITAL, ANNUAL_HH_INCOME, DEPRESSED)%>%
      dplyr::filter(MARITAL%in%(input$marital), !ANNUAL_HH_INCOME%in%c("null", "Under $20,000", "$20,000 + "), DEPRESSED!="null" )%>%
      dplyr::group_by(MARITAL, ANNUAL_HH_INCOME, DEPRESSED)%>%
      dplyr::summarize(count = n())%>%dplyr::mutate(percentage = count/sum(count), pos = (cumsum(percentage) - 0.5 * percentage))
    #mapping strings to factors for income brackets for ordering
    df$scode<-mapvalues(df$ANNUAL_HH_INCOME, from = c("$0 - 4,999", "$5,000 - 9,999", "$10,000 - 14,999", "$15,000 - 19,999", "$20,000 - 24,999", "$25,000 - 34,999", "$35,000 - 44,999", "$45,000 - 54,999", "$55,000 - 64,999", "$65,000 - 74,999", "$75,000 - 99,999", "$100,000 +"), to=c("1", "2", "3", "4", "5", "6","7", "8", "9","10", "11", "12"))
    
      p<-ggplot(data= df, aes(x =ANNUAL_HH_INCOME, y=percentage, fill=DEPRESSED)) + geom_bar(stat="identity", position ="stack") + labs(title = "") + ggtitle("") + theme(legend.title =element_text(size=8), legend.text = element_text(size=6), plot.title=element_text(size=10, face="bold"), axis.title.x = element_text(size=7), axis.text=element_text(size=6))+ geom_text(data = subset(df,percentage>=0.05), aes(y=pos, label = paste(round(percentage*100,1), "%")), size=3, color="black")+ scale_x_discrete(limits= c("$0 - 4,999", "$5,000 - 9,999", "$10,000 - 14,999", "$15,000 - 19,999", "$20,000 - 24,999", "$25,000 - 34,999", "$35,000 - 44,999", "$45,000 - 54,999", "$55,000 - 64,999", "$65,000 - 74,999", "$75,000 - 99,999", "$100,000 +"), expand=c(0,-2)) + xlab("") + ylab("% of Total Count Depressed") + scale_y_continuous(expand=c(0,0)) + coord_flip() 
ggplotly(p)%>%layout(margin=list(b=150))
})
```
###**Tab 2: Box Plot Using Shiny and Plotly HTML Widget**
### Box Plot: Ratio Income Poverty v. Severe Depression by Gender
```{r, echo=FALSE}
df1<- read.csv("mentalHealth.reformatted.csv", stringsAsFactors = FALSE)
df2<- read.csv("demographics.reformatted.csv", stringsAsFactors = FALSE)
df<-merge(df1, df2)
df$scode<-mapvalues(df$ANNUAL_HH_INCOME, from = c("Nearly every day", "More than half the days", "Several days", "Never"), to=c("1", "2", "3", "4"))
selectInput(input = "gender",
              label = "Select a Gender: ",
              c("Male", "Female"), selected = "Male")
plotlyOutput("boxplot", width = "auto", height = "auto")
output$boxplot<-renderPlotly({
  df1<- read.csv("mentalHealth.reformatted.csv", stringsAsFactors = FALSE)
  df2<- read.csv("demographics.reformatted.csv", stringsAsFactors = FALSE)
  df<-merge(df1, df2)
      df <-df %>%
      dplyr::select(GENDER, RATIO_INCOME_POVERTY, BETTER_DEAD)%>%
      dplyr::filter(GENDER%in%c(input$gender), RATIO_INCOME_POVERTY!="null",BETTER_DEAD!="null")%>%
      dplyr::group_by(GENDER, RATIO_INCOME_POVERTY, BETTER_DEAD)
      p<-ggplot(data= df, aes(x =BETTER_DEAD, y=as.numeric(as.character(RATIO_INCOME_POVERTY)), fill=BETTER_DEAD)) + geom_boxplot() + xlim(c("Nearly every day", "More than half the days", "Several days", "Never")) + ylim(0,5) + xlab("Often Feels Better Dead") + ylab("Ratio Income Poverty") + theme(legend.position="none")
ggplotly(p)%>%layout(margin=list(b=150))
})
    

```
