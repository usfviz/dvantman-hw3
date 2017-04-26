library(shiny)
library(d3heatmap)
library(lubridate)
library(plotly)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(GGally)
library(tidyr)

#setwd("~/Documents/USF/Spring_Module_2/Data_Visualization/Asg3/Facebook_metrics")

###########################
### HEATMAP DATA###########
###########################

data <- read.csv("dataset_Facebook.csv")

data$Post.Weekday[data$Post.Weekday == 1] <- 'Mon'
data$Post.Weekday[data$Post.Weekday == 2] <- 'Tue'
data$Post.Weekday[data$Post.Weekday == 3] <- 'Wed'
data$Post.Weekday[data$Post.Weekday == 4] <- 'Thu'
data$Post.Weekday[data$Post.Weekday == 5] <- 'Fri'
data$Post.Weekday[data$Post.Weekday == 6] <- 'Sat'
data$Post.Weekday[data$Post.Weekday == 7] <- 'Sun'

data$Post.Month[data$Post.Month == 1] <- 'Jan'
data$Post.Month[data$Post.Month == 2] <- 'Feb'
data$Post.Month[data$Post.Month == 3] <- 'Mar'
data$Post.Month[data$Post.Month == 4] <- 'Apr'
data$Post.Month[data$Post.Month == 5] <- 'May'
data$Post.Month[data$Post.Month == 6] <- 'Jun'
data$Post.Month[data$Post.Month == 7] <- 'Jul'
data$Post.Month[data$Post.Month == 8] <- 'Aug'
data$Post.Month[data$Post.Month == 9] <- 'Sep'
data$Post.Month[data$Post.Month == 10] <- 'Oct'
data$Post.Month[data$Post.Month == 11] <- 'Nov'
data$Post.Month[data$Post.Month == 12] <- 'Dec'

new_df <- subset(data, select = c("Post.Weekday", "Post.Month", "Lifetime.Post.Total.Reach","Lifetime.Post.Total.Impressions","Lifetime.Engaged.Users"))
new_df$Post.Weekday[new_df$Post.Weekday == 'Mon'] <- 'Monday'
new_df$Post.Weekday[new_df$Post.Weekday == 'Tue'] <- 'Tuesday'
new_df$Post.Weekday[new_df$Post.Weekday == 'Wed'] <- 'Wednesday'
new_df$Post.Weekday[new_df$Post.Weekday == 'Thu'] <- 'Thursday'
new_df$Post.Weekday[new_df$Post.Weekday == 'Fri'] <- 'Friday'
new_df$Post.Weekday[new_df$Post.Weekday == 'Sat'] <- 'Saturday'
new_df$Post.Weekday[new_df$Post.Weekday == 'Sun'] <- 'Sunday'

new_df$Post.Weekday <- as.factor(new_df$Post.Weekday)
new_df$Post.Month <- as.factor(new_df$Post.Month)

agg_df <- aggregate(.~Post.Weekday+Post.Month, new_df, sum)
colnames(agg_df) <- c("Day_of_Week","Month","Total Reach","Total Impressions","Engaged Users")
df_all <- melt(agg_df)
colnames(df_all) <- c("Day_of_Week","Month","metric","Total")

# agg_df1 <- aggregate(new_df$Lifetime.Post.Total.Reach, by = list(new_df$Post.Weekday, new_df$Post.Month), sum)
df_all$Day_of_Week <- factor(df_all$Day_of_Week, levels= c("Sunday", "Monday",
                                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

df_all$month_text <- paste("Month: "  , df_all$Month, sep="")
df_all$day_text <- paste("Day of the Week: ", df_all$Day_of_Week, sep = "")
df_all$total_text <- paste("Total: ", df_all$Total, sep = "")
df_all$month_date <- paste(df_all$month_text,df_all$day_text, sep = "<br>")
df_all$all_text<- paste(df_all$month_date,df_all$total_text, sep = "<br>")

# 
df_all$Month <- factor(df_all$Month, levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                               "Aug","Sep", "Oct", "Nov", "Dec"))

###########################
###SMALL MATRICES DATA#####
###########################

subset_data <- data[, names(data) %in% c("Lifetime.Post.Total.Impressions", "comment","like","share","Post.Month"
                                         ,"Lifetime.Engaged.Users")]
agg_df1 <- aggregate(.~Post.Month, subset_data, mean)
agg_df1$Post.Month <- as.factor(agg_df1$Post.Month)
agg_df1$Post.Month <- factor(agg_df1$Post.Month, levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                         "Aug","Sep", "Oct", "Nov", "Dec"))

colnames(agg_df1) <- c("Months","Avg Impressions","Avg Comments","Avg Likes","Avg Shares","Avg Lifetime Engaged Users")

melt_df <- melt(agg_df1)
melt_df$id <- 1
melt_df$id[melt_df$variable == 'Avg Comments'] <- 2
melt_df$id[melt_df$variable == 'Avg Likes'] <- 3
melt_df$id[melt_df$variable == 'Avg Shares'] <- 4
melt_df$id[melt_df$variable == 'Avg Lifetime Engaged Users'] <- 5


###########################
### PARALLEL PLOT DATA####
###########################


df_par <- subset(data, select = c("Type","Category","Post.Month","Post.Weekday","Paid"))
colnames(df_par) <- c("Type","Category","Months","Day of Week", "Paid")
df_par$Paid <- as.factor(df_par$Paid)
df_par$category_levels[df_par$Type == 'Link'] <- 1
df_par$category_levels[df_par$Type == 'Photo'] <- 2
df_par$category_levels[df_par$Type == 'Status'] <- 3
df_par$category_levels[df_par$Type == 'Video'] <- 4

df_par <- na.omit(df_par)

ui <- fluidPage(
  headerPanel("Facebook Interactive Plots \n\n"),
  mainPanel(
    tabsetPanel(
      tabPanel("Heatmap", plotlyOutput("heatmap"),
               sidebarPanel(
                 selectInput("metric", "Select Metric", choices = levels(df_all$metric))
               )),
      tabPanel("Small Multiple Plot", plotlyOutput("smallmatrices")), 
      tabPanel("Parallel Coordinates", plotlyOutput("parallel"),
               sidebarPanel(
                 selectInput("type", "Select Type", choices = append(list("All"),levels(df_par$Type))))
    )
  )
)
)

server <- function(input, output) {
  
  data <- reactive({df_all %>% filter(metric == input$metric)})
  
  output$heatmap <- renderPlotly({
    p <- plot_ly(data=data(),
                 x = ~Day_of_Week, y = ~Month,
                 z = ~Total, type = "heatmap", text=~all_text, hoverinfo = "text",
                 colors = c('#ffffcc','#41b6c4','#2c7fb8','#253494')
    )
  })
  
  output$smallmatrices <- renderPlotly({
    p <- melt_df %>%
      transform(id = as.integer(factor(variable))) %>%
      plot_ly(x = ~Months, y = ~value, color = ~variable, colors = "Dark2",
              yaxis = ~paste0("y", id)) %>%
      add_lines() %>%
      subplot(nrows = 5, shareX = TRUE)
  })
  output$parallel <- renderPlotly({
    
  })
  df <- reactive({
    if(input$type == 'All'){
      df <- df_par
    }
    else{
      df <- df_par %>% filter(Type == input$type)}
    })
  
  output$parallel <- renderPlotly({
    plt <- ggparcoord(df(), columns = 1:4, groupColumn = "Paid",scale = 'uniminmax', mapping = c("0","1")) +
      scale_colour_manual(values = c("0" = "tomato","1" = "turquoise3")) +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y=element_blank(),
            axis.text.x = element_text(colour = 'turquoise4'),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_rect(fill="white",color = "black", size = 0.5),
            panel.grid.major = element_blank())
    ggplotly(plt, tooltip = c('colour'))
})
}

shinyApp(ui = ui, server = server)



