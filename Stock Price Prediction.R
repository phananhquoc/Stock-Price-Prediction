library(shinydashboard)
library(dplyr)
library(shiny)
library(ggplot2)  
library(MASS)
library(ggcorrplot)
library(shinyWidgets)
library(ggpubr)
library(performance)
library(tidyverse)
library(broom)
#library(shinydashboardPlus)
# Định nghĩa hàm "create_train_test()"
create_train_test <- function(data, train_prop = 0.8, seed = 123) {
  # Tách dữ liệu thành tập huấn luyện và tập kiểm định
  set.seed(seed)
  n <- nrow(data)
  train_size <- floor(train_prop * n)
  train_index <- sample(seq_len(n), size = train_size)
  train <- data[train_index, ]
  test <- data[-train_index, ]
  
  # Trả về danh sách chứa tập huấn luyện và tập kiểm định
  return(list(train = train, test = test))
}

# Định nghĩa hàm "read_train_test_csv()" cho ứng dụng Shiny
read_train_test_csv <- function(upload, train_prop = 0.8, seed = 123) {
  # Đọc dữ liệu từ tệp CSV
  data <- read.csv(upload)
  
  # Tách dữ liệu thành tập huấn luyện và tập kiểm định
  train_test_data <- create_train_test(data, train_prop = train_prop, seed = seed)
  
  # Trả về danh sách chứa tập huấn luyện và tập kiểm định
  return(train_test_data)
}


ui <- dashboardPage( skin = "green",
  dashboardHeader(title = "Stock Price Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("dashboard")),
      menuItem("Model", tabName = "model", icon = icon("th")),
      menuItem("Prediction", tabName = "pred", icon = icon("signal")),
      menuItem("About Us", tabName = "doc", icon = icon("file"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "data",
              tabBox(
                id = "tabset1", width = 12, height = 610,
                tabPanel("Summary",
                         box(title = "Upload",status = "danger", solidHeader = TRUE, collapsible = TRUE,
                           fileInput("upload", NULL, accept = c(".csv")),
                           numericInput(
                             "train_prop", label = h4("Training ratio"),
                             min = 0.1, max = 0.9, value = 0.8, step = 0.1
                           ),
                           actionButton("run_button", label = "Run"),
                           #checkboxGroupInput("column", "Choose column", character(0)),
                           uiOutput("cot")
                         ), 
                         box(title = "Summary",status = "danger", solidHeader = TRUE, collapsible = TRUE, verbatimTextOutput("mytable1")),),
                tabPanel("Structure", verbatimTextOutput("ta")),
                tabPanel("Data",  
                          box(title = "Select variable", status = "warning", solidHeader = TRUE, width = 4,
                                               collapsible = TRUE,
                         uiOutput("vb", height = 250)
                         ),
                         box(title = "Dataset",status = "warning", solidHeader = TRUE, width = 8,collapsible = TRUE,
                         DT::dataTableOutput("head")
                         )
                ),
                tabPanel("Plot",
                         box(title = "Select the Plot of your choice", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                         selectInput(inputId = "dat", label = "", choices = c("Boxplot","Histogram", "Correlation matrix", "Scatterplots")),
                         uiOutput("vz"),
                         uiOutput("vt"),),
                         
                         box(title= "Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                           tabPanel("Plot", plotOutput("mytable2")),
                         )
                         
                         ),       
              ),
              
      ),
      
      # Second tab content
      tabItem(tabName = "model",
              fluidPage(
                box(title = "Select variable",status = "info", solidHeader = TRUE,collapsible = TRUE, 
                  uiOutput("vx"), 
                  uiOutput("vy"),
                ),
                box(title = "Formula", status = "info", solidHeader = TRUE,collapsible = TRUE,
                  headerPanel("LINEAR REGRESSION FORMULA IS: "), verbatimTextOutput("plot1"),  
                  verbatimTextOutput("plot2"),
                ),
                box(title = "Plot",status = "info", solidHeader = TRUE,collapsible = TRUE, 
                  plotOutput("plot3"),
                )
              )
      ),
      tabItem(tabName = "pred",
              h1("Prediction of Stock"),
              fluidRow(
               box(title = "Choose number new data set ID", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE,
                 textInput("so", label = "", value = 10)
               ),
               box(title = "Results", status = "success", solidHeader = TRUE, width = 6, collapsible = TRUE,
                            tabPanel("Predict", headerPanel("THE STOCK PRICE IS:"), tableOutput("value")) 
              
              )
              )# close flui row
                   
               # close box 2
      ),# close tabitem pred
      tabItem(tabName = "doc",
              verbatimTextOutput("doc")
              
             )
      
      
    )
  )
)
server <- function(input, output) {
  
  
  #data1 <- reactive({
   # options(shiny.maxRequestSize=30*1024^2)
    #req(input$upload)
    #ext <- tools::file_ext(input$upload$name)
    #switch(ext,
    #       csv = vroom::vroom(input$upload$datapath, delim = ","),
     #      tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
      #     validate("Invalid file; Please upload a .csv or .tsv file")
    #)
  #})
  # Đọc tệp CSV và tách thành tập huấn luyện và tập kiểm định khi người dùng bấm nút "Run"
  data1 <- eventReactive(input$run_button, {
    req(input$upload$datapath)
    read_train_test_csv(input$upload$datapath, train_prop = input$train_prop)
  })
  output$cot <- renderUI({
    req(data1()$train)
    ka<-colnames(data1()$train)
    checkboxGroupInput(
      inputId = "checkgroup4",
      label = "Select variable",
      choices = c(ka[1:length(ka)]),
      #icon = icon("check"),
      selected = ka[1],
      #animation = "tada",
      #status = "default"
    )  
  })
  #observeEvent(input$upload, {
  #  updateCheckboxGroupInput(inputId = "column", choices = names(data1()$train))
  #})
  output$mytable1 <- renderPrint({
    options(width= 100)
    summary(data1()$train[c(input$checkgroup4)])
  })
  output$ta <- renderPrint({
    str(data1()$train)
  })
  output$head <- DT::renderDataTable({
    data1()$train[c(input$checkgroup2)]
  })
  output$vb<- renderUI({
    req(data1()$train)
    ba<-colnames(data1()$train)
    prettyCheckboxGroup(
      inputId = "checkgroup2",
      label = "",
      choices = c(ba[1:length(ba)]),
      icon = icon("check"),
      selected = ba[1],
      animation = "tada",
      status = "default"
    )
  })
  
  output$vx <- renderUI({
    req(data1()$train)
    xa<-colnames(data1()$train) 
    pickerInput(inputId = 'variablex',
                label = 'Select y-axis variable',
                choices = c(xa[1:length(xa)]), selected=xa[5],
                options = list(`style` = "btn-info"))
  })
  
  output$vy <- renderUI({
    req(data1()$train)
    ya<-colnames(data1()$train) 
    pickerInput(inputId = 'variabley',
                label = 'Select x-axis variable',
                choices = c(ya[1:length(ya)]), selected=ya[2],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
  })
  # trả về các lựa chọn biến thứ nhất tương ứng với biểu đồ đã chọn trong tab plot
  output$vz <- renderUI({
    req(data1()$train)
    za<-colnames(data1()$train)
    if (input$dat == "Boxplot"){
      prettyCheckboxGroup(
        inputId = "checkgroup3",
        label = "Select variable",
        choices = c(za[1:length(za)]),
        icon = icon("check"),
        selected = za[2],
        animation = "tada",
        status = "default"
      )
    } else if (input$dat == "Histogram") {
      radioButtons(
        inputId = "somevalue3",
        label = "With column:",
        choices = c(za[1:length(za)]),
        selected = za[2]
      )
    } else if (input$dat =="Scatterplots") {
      selectInput(inputId = "select1", label = "Choose one variable", choices = c(za[1:length(za)]), selected = za[2])
    }
  })
  # trả về các lựa chọn biến thứ nhất tương ứng với biểu đồ đã chọn trong tab plot
  output$vt <- renderUI({
    req(data1()$train)
    ta<-colnames(data1()$train)
    if (input$dat == "Histogram"){
      sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 50, value = 30)
    } else if (input$dat =="Scatterplots") {
      selectInput(inputId = "select2", label = "Choose one variable", choices = c(ta[1:length(ta)]), selected = ta[2])
    }
  })
  output$mytable2 <- renderPlot({
    #Hiển thị đồ thị boxplot khi chọn Boxplot
    if (input$dat == "Boxplot") {
      boxplot(data1()$train[c(input$checkgroup3)], main="Boxpolt of the dataset")
    } 
    #Hiển thị đồ thị histogram khi chọn Histogram
    else if (input$dat == "Histogram") {
      x <- as.numeric(data1()$train[[as.name(input$somevalue3)]])
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, main = "Histogram of Dataset", breaks = bins,col = "blue", xlab = as.name(input$somevalue3))
    }
    else if (input$dat == "Correlation matrix") {
      corr <- round(cor(data1()$train), 1) # hàm tính giá trị tương quan giữa các biến
      ggcorrplot(corr,
                 type = "lower",
                 lab = TRUE, 
                 lab_size = 5,  
                 colors = c("tomato2", "white", "springgreen3"),
                 title="Correlogram of Dataset", 
                 ggtheme=theme_bw)
    } else if (input$dat == "Scatterplots"){
      req(lmModel())
      a <- as.numeric(data1()$train[[as.name(input$select1)]])
      b <- as.numeric(data1()$train[[as.name(input$select2)]])
      plot(x = a, y = b,
           xlab = as.name(input$select1),
           ylab = as.name(input$select2),
           col = "purple",
           main = "Scatterplots of Dataset"
      )
    } 
  })
  lmModel <- reactive({
    req(data1()$train,input$variablex,input$variabley)
    x <- as.numeric(data1()$train[[as.name(input$variablex)]])
    y <- as.numeric(data1()$train[[as.name(input$variabley)]])
    current_formula <- paste0(input$variablex, " ~ ", paste0(input$variabley, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = data1()$train)
    return(model)
  })
  #xử lý trả về công thức
  bl<-reactive({
    req(data1()$train,input$variablex,input$variabley)
    x <- as.numeric(data1()$train[[as.name(input$variablex)]])
    y <- as.numeric(data1()$train[[as.name(input$variabley)]])
    current_formula <- paste0(input$variablex, " ~ ", paste0(input$variabley, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = data1()$train)
    model %>%
        #hàm tidy() sắp xếp các biến 
        tidy() %>% 
        #hàm mutate() thêm các biến mới và giữ nguyên các biến hiện có
        mutate( term = if_else(term == "(Intercept)", "", term),
                sign = case_when( term == "" ~ "",estimate < 0 ~ "-",estimate >= 0 ~ "+"),
                estimate = as.character(round(abs(estimate), digits = 2)),
                term = if_else(term == "", paste(sign, estimate), paste(sign, estimate, term))
        )%>%
        #summarise()tạo một khung dữ liệu mới.Nó sẽ có một (hoặc nhiều) hàng cho mỗi sự kết hợp của các biến nhóm
        summarize(terms = paste(term, collapse = " ")) %>%
        #kéo ra công thức
        pull(terms)
    
  })
  # in ra phương trình hồi quy 
  output$plot1 <- renderPrint({
    bl()
  })
  #đồ thị hồi quy tuyến tính của model
  output$plot3 <- renderPlot({
    par(mfrow = c(2,2))
    plot(lmModel())
  }) 
dd<- reactive({
  #predictions <- predict(lmModel(), newdata= test_data)
  preds <- data.frame(predict(lmModel(), newdata = data1()$test))
  preds$row_num <- seq.int(nrow(preds)) 
  names(preds) <- c('Price Predictions', 'New Dataset ID')
  return(preds)
  #predictions >-1 
  #print(predictions)
 
})
output$doc <- renderPrint({
  ("Dự án được thực hiện bởi Phan Anh Quốc, sinh viên trường Đại học công nghệ thông tin và truyền thông Việt Hàn")
})
  # đưa ra giá nhà dự đoán 
output$value <- renderTable({
 dd()[1:input$so,]
  
})
}
shinyApp(ui, server)