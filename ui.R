
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Demand System Analyses (ICP 2011). Author: Long Vo"),
  
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
      fileInput("file3", "Upload Budget-Share Data Here",
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
                   selected = "all")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("PPP", tableOutput("ppp")),
        tabPanel("Expenditure", tableOutput("exp")),
        tabPanel("Budget Share", tableOutput("bs")),
        tabPanel("Q Disp. Table", tableOutput("q_disp")),
        #
        tabPanel("Food Price & Quantity Density Plot", plotOutput("density"),
                 withMathJax(),
                 helpText('Note:',
                          br(),
                          'This plot presents the densities of the log-differences 
                          in the quantities of good \\(i\\) consumed and the corresponding relative 
                          price differences between countries \\(c\\) and \\(d\\), for \\(c,d=1,...,176, d>c\\).
                          Quantities are adjusted for income differences (with an income elasticity of \\(\\frac{1}{2}\\) ), 
                          that is, \\(100 (Dq_{i,cd}-\\frac{1}{2}\\ log Q_{cd}) 
                          = 100 (log \\frac{q_{ic}}{Q^{1/2}_c} - log \\frac{q_{id}}{Q^{1/2}_d} )\\) for \\(i\\). 
                          The relative price difference is \\( 100(Dp_{i,cd}-logP_{cd})=100(log\\frac{p_{ic}}{P_c} - log \\frac{p_{id}}{P_d} ) \\).',
                 )),
        tabPanel("Food Demand Curve Plot", plotOutput("demand_plot"),
              withMathJax(),
              helpText('Note:',
                 br(),
                 br(),
                 'The points underlying this heat map are the log-differences 
                 in the quantities of good \\(i\\) consumed and the corresponding relative 
                 price differences between countries \\(c\\) and \\(d\\), for \\(c,d=1,...,176, d>c\\).
                 Quantities are adjusted for income differences (with an income elasticity of \\(\\frac{1}{2}\\) ), 
                 that is, \\(100 (Dq_{i,cd}-\\frac{1}{2}\\ log Q_{cd}) 
                 = 100 (log \\frac{q_{ic}}{Q^{1/2}_c} - log \\frac{q_{id}}{Q^{1/2}_d} )\\) for \\(i\\). 
                 The relative price difference is \\( 100(Dp_{i,cd}-logP_{cd})=100(log\\frac{p_{ic}}{P_c} - log \\frac{p_{id}}{P_d} ) \\).',
                 br(),
                 br(),
                 '2. This is a heat map in which "redder" colours represent denser groups of observations.',
                 br(),
                 br(),
                 'Data source: Clements, K. W. (2019). "', a("Four Laws of Demand",href="https://onlinelibrary.wiley.com/doi/full/10.1111/1475-4932.12491", target="_blank"),
                 '". Economic Record 95 (310): 358-385.'
                 )),
        #
        tabPanel("Dispersion Plot", plotOutput("disp_plot"),
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
                    is the Divisia index of prices in \\(c\\) relative to \\(d\\).',
                    br(),
                    br(),
                    '2. This is a heat map in which "redder" colours represent denser groups of observations.',
                    br(),
                    br(),
                    'Data source: Clements, K. W. (2019). "', a("Four Laws of Demand",href="https://onlinelibrary.wiley.com/doi/full/10.1111/1475-4932.12491", target="_blank"),
                    '". Economic Record 95 (310): 358-385.'
                 ))
          )
   )
 )
)
