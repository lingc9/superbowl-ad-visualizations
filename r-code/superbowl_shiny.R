#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')
youtube$like_ratio <- youtube$like_count/(youtube$like_count + youtube$dislike_count)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Super Bowl EDA Plot"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("brand", "Brand Group",
                   choices = list("Cars" = "Cars",
                                  "Hard Drinks" = "Hard Drinks",
                                  "Soft Drinks" = "Soft Drinks",
                                  "Food" = "Food",
                                  "NFL" = "NFL",
                                  "E-Trade" = "E-Trade")),
      # selectInput('x', 'x axis:',
      #             choices = list("view_count" = "view_count",
      #                            "like_count" = "like_count",
      #                            "dislike_count" = "dislike_count",
      #                            "like_ratio" = "like_ratio")),
      selectInput('y', 'Video Statistics:',
                  choices = list("View Count" = "view_count",
                                 "Like Count" = "like_count",
                                 "Dislike Count" = "dislike_count",
                                 "Like Ratio" = "like_ratio")),
      # selectInput('marginal', 'Margianl distribution of:',
      #             choices = list("brand" = "brand",
      #                            "year" = "year",
      #                            "catergory_id" = "category_id")),
      selectInput('fill', 'Fill Factors:',
                  choices = list("Funny" = "funny",
                                 "Show Product Quickly" = "show_product_quickly",
                                 "Patriotic" = "patriotic",
                                 "Celebrity" = "celebrity",
                                 "Danger" = "danger",
                                 "Animals" = "animals",
                                 "Use Sex" = "use_sex"))
      
    ),
    
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("marginalplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      youtube_red <- youtube[,c(1:2, 5:11, 15:17, 19, 25)]
      youtube_red$like_ratio <- youtube$like_count/(youtube$like_count + youtube$dislike_count)
      youtube_red <- youtube_red %>%
        mutate(view_category = case_when(
          view_count < 4000 ~ "Few\nLess than 4K",
          view_count >= 4000 & view_count < 30000 ~ "Some\n4K to 30K",
          view_count >= 30000 & view_count < 90000 ~ "Moderate\n30K to 90K",
          view_count >= 90000 & view_count < 500000 ~ "Many\n90K to 500K",
          view_count >= 500000 & view_count < 10000000 ~ "High\n500K to 10M",
          TRUE ~ "Viral\n10M+"),
          brand_category = case_when(
            brand == "Hynudai" ~ "Cars", 
            brand == "Kia" ~ "Cars",
            brand == "Toyota" ~ "Cars",
            brand == "Bud Light" ~ "Hard Drinks", 
            brand == "Budweiser" ~ "Hard Drinks",
            brand == "Coca-Cola" ~ "Soft Drinks", 
            brand == "Pepsi" ~ "Soft Drinks",
            brand == "Doritos" ~ "Food",
            brand == "NFL" ~ "NFL",
            brand == "E-Trade" ~ "E-Trade"),
          alt_brand_category = case_when(
            brand == "Hynudai" ~ "Cars",
            brand == "Kia" ~ "Cars", 
            brand == "Toyota" ~ "Cars",
            brand == "Bud Light" ~ "Drinks", 
            brand == "Budweiser" ~ "Drinks", 
            brand == "Coca-Cola" ~ "Drinks", 
            brand == "Pepsi" ~ "Drinks",
            brand == "Doritos" ~ "Misc.", 
            brand == "NFL"  ~ "Misc.", 
            brand == "E-Trade" ~ "Misc."))
      youtube_brand <- filter(youtube_red, youtube_red$brand_category == input$brand)
      youtube_brand %>%
        ggplot(aes(x = year, 
                   y = eval(parse(text = input$y)), 
                   color = eval(parse(text = input$fill)))) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", level = 0.95) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "Year", 
             y = str_to_title(gsub('[_]', ' ', input$y)), 
             col = str_to_title(gsub('[_]', ' ', input$fill)),
             title = paste( input$brand, "Brand's", str_to_title(gsub('[_]', ' ', input$y)), "over Time"))
     })
    
    output$marginalplot <- renderPlot({
      youtube %>%
        ggplot(aes(x = year,
                   fill = eval(parse(text = input$fill)))) +
        geom_bar() +
        facet_wrap(~ brand) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "Year",
             y = "Count",
             fill = str_to_title(gsub('[_]', ' ', input$fill)),
             title = paste("Conditional Distribution of Year given", str_to_title(gsub('[_]', ' ', input$fill))))
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
