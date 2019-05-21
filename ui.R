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
        tabPanel("P vs Q Disp. Plot", plotOutput("newplot"),
              withMathJax(),
              helpText('Notes:',
                 br(),
                 br(),
                 '1. 	Points here represent the standard deviations (SD) of quantities 
                 and prices (both \\(\\times 100\\)) for each pair of countries \\(c\\) and \\(d,\\)
                 where \\(c,d=1,\\dots,176,c<d\\). The SD of quantities is the square root of the 
                 Divisia variance of quantity differences between countries \\(c\\) and \\(d\\), 
                 \\(\\sigma^2_{q,cd} =\\sum^9_{i=1} w_{i,cd}\\left(Dq_{i,cd}-DQ_{cd}\\right)^2\\) 
                 where \\(w_{i,cd}=\\frac{1}{2}\\left(w_{ic}+w_{id}\\right)\\) is the average budget share of good
                 \\(i\\) in \\(c\\) and \\(d\\);  
                 \\(Dq_{i,cd}=log q_{ic}-log q_{id}\\) 
                 is the log-difference between per capita consumption of good \\(i\\) in \\(c\\) and \\(d\\)
                 and \\(DQ_{cd}=\\sum^9_{i=1} w_{i,cd}Dq_{i,cd}\\) is the Divisia volume index 
                 for \\(c\\) relative to \\(d\\).
                 The SD of prices is the square root of the Divisia variance of price differences, \\(\\sigma^2_{p,cd}=\\sum^9_{i=1}w_{i,cd}\\left(Dp_{i,cd}-DP_{cd}\\right)^2\\) 
                 where \\(Dp_{i,cd}=log p_{ic}-log p_{id}\\) is the log-difference between the 
                 price of \\(i\\) in \\(c\\) and \\(d\\); and \\(DP_{cd}=\\sum^9_{i=1}w_{i,cd}Dp_{i,cd}\\) 
                 is the Divisia index of prices in \\(c\\) relative to \\(d\\).',
                 br(),
                 br(),
                 '2. This is a heat map in which "redder" colours represent denser groups of observations.'
                 ))
      )
   )
     
 )
))
