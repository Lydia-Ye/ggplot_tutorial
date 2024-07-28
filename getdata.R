library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)

data.all <- read_csv("https://www.stat2games.sites.grinnell.edu/data/greenhouse/getdata.php") 
data.all <- select(data.all, GroupID, PlayerID, Level, Season, SellPrice, BuyPrice, Crop, Money, Water, Nitrates, Yield, Profit)
sample1 <- read.csv("Sample1_data.csv")
sample1 <- select(sample1, GroupID, PlayerID, Level, Season, SellPrice, BuyPrice, Crop, Money, Water, Nitrates, Yield, Profit)

data.all <- rbind(data.all, sample1)

# Revenue/Costs/Profit
data.all <- data.all %>% mutate(
  Revenue = Money,
  Costs = BuyPrice + Water + ifelse((Nitrates - 250) / 10 < 0, 0, (Nitrates - 250) / 10),
  Profit = Revenue - Costs
)

data.all$Level <- as.factor(data.all$Level)
data.all$GroupID <- tolower(data.all$GroupID)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)

all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))

# UI
ui <- fluidPage(
  
  titlePanel("Get Greenhouse Data"),
    mainPanel(
      
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices = c("all", all_groups),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      downloadButton('downloadData', label = "Download Data")
    )
)

# Server
server <- function(input, output, session) {

  # Reactive expression to store filtered data
  filteredData <- reactive({
    req(data.all)  # Ensure the dataset is loaded
    
    # Filter by Group ID
    if (length(input$groupID) > 0) {
      if ("all" %in% input$groupID) {
        data <- data.all
      } else {
        data <- data.all[data.all$GroupID %in% input$groupID, ]
      }
    } else {
      data <- data.all  # No filtering if no group selected
    }
    
    data
  })

  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(filteredData(), con)
    }
  )
}

# Running Shiny App
shinyApp(ui = ui, server = server)