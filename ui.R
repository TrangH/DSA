library(shiny)

# Define UI for data upload app ----
shinyUI(fluidPage(
  
  # App title ----
  titlePanel("Demand System Analyses"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Upload PPP Data Here",
                multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
      fileInput("file2", "Upload Expenditure Data Here",
                multiple = TRUE, 
                accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
      fileInput("file3", "Upload Budget Share Data Here",
                multiple = TRUE, 
                accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(`First 5 rows` = "head",
                               `All rows` = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("PPP", tableOutput("ppp")),
        tabPanel("Expenditure", tableOutput("exp")),
        tabPanel("Budget Share", tableOutput("bsh")),
        tabPanel("Q Disp. Table", tableOutput("q_disp")),
        tabPanel("P vs Q Disp. Plot", plotOutput("newplot"))
      )
    )
  )
))
