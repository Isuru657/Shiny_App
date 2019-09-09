library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
theme <- theme_classic()+
  theme(axis.text.x.bottom = element_text(angle = 90))+
  theme(plot.caption = element_text(face = "italic", color = "#7F8C8D"))+
  theme(panel.grid.major = element_line(linetype = 2))
# Define UI for data upload app ----
ui <- fluidPage(
  titlePanel(title = h1("Data Analyzer", align = "center")),
  navbarPage(title="Navigation menu",
    tabPanel(title="About",
      includeMarkdown("/Users/isuruabeysekara/Desktop/about.Rmd")
    ),
    tabPanel(
      title="Data",
      sidebarLayout(
        sidebarPanel(
          fileInput("uploaded_file", "Choose CSV File",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          tags$hr(),
          checkboxInput("header", "Header", TRUE),
          radioButtons("sep", "Separator",
                       choices = c(Semicolon = ";",
                                   Comma = ",",
                                   Tab = "\t"),
                       selected = ","),
          tags$hr(),
          radioButtons("disp", "Display",
                       choices = c(All = "all",
                                   Head = "head"),
                       selected = "all"),
          uiOutput("checkbox6"),
          downloadButton(outputId = "button1", label = "Download")
        ),
        mainPanel(
          DT::dataTableOutput("rendered_file"))
      )
    ),
    tabPanel(title="Total Count",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file2", "Choose CSV File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 uiOutput("checkbox1"),
                 uiOutput("checkbox2"),
                 checkboxInput("check", "Seasonal Changes?", value= FALSE),
                 textInput(inputId = "filename2", label="Save data as"),
                 downloadButton(outputId = "button3", label="Download")
               ),
               mainPanel(
                 textInput(inputId = "title", label="Enter plot title"),
                 textInput(inputId= "xlab", label="Enter X axis label"),
                 textInput(inputId="ylab", label="Enter Y axis label"),
                 plotOutput("plot"),
                 DT::dataTableOutput("table")
               )
             )
    ),
    tabPanel(title="Total Revenue",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 uiOutput("checkbox3"),
                 uiOutput("checkbox4"),
                 checkboxInput("check1", "Seasonal Changes?", value= FALSE),
                 uiOutput("checkbox5"),
                 textInput(inputId = "filename4", label="Save data as"),
                 downloadButton(outputId = "button5", label = "Download")
               ),
               mainPanel(
                 textInput(inputId = "title1", label="Enter plot title"),
                 textInput(inputId= "xlab1", label="Enter X axis label"),
                 textInput(inputId="ylab1", label="Enter Y axis label"),
                 plotOutput("plot1"),
                 DT::dataTableOutput("table1")
               )
             ))
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  df <- reactive({
    req(input$uploaded_file)
    read.csv(input$uploaded_file$datapath,
             header = input$header,
             sep = input$sep)  
    
  })

  output$checkbox6 <- renderUI({
    checkboxGroupInput(inputId = "select_var8", 
                       label = "Select variables", 
                       choices = names(df()))
  })
  

  df_sel <- reactive({
    req(input$select_var8)
    df_sel <- df() %>% select(input$select_var8)
  })
  

  output$rendered_file <- DT::renderDataTable({
    if(input$disp == "head") {
      head(df_sel())
    }
    else {
      df_sel()
    }
  })
  
  output$button1 <- downloadHandler(
    filename= function(){
      paste("filename", "csv", sep=".")
    },
    content= function(file){
      write.csv(df_sel(),file)
    }
  )
  #Total Revenue Section ------------
  df1 <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,
             header = TRUE,
             stringsAsFactors = FALSE,
             strip.white = TRUE)  
  })
  output$checkbox4 <- renderUI({
    checkboxGroupInput(inputId = "select_var3", 
                       label = "Select variable containing Revenue", 
                       choices = names(df1()))})
  output$checkbox3 <- renderUI({
    checkboxGroupInput(inputId = "select_var2", 
                       label = "Select variables", 
                       choices = names(df1()))})
  
  output$checkbox5 <- renderUI({
    if(input$check1==TRUE){
      checkboxGroupInput(inputId= "select_var4",
                         label="Select Time Variable",
                         choices= names(df1()))}
    else{return()}
  })
  df_sel1 <- reactive({
    if(input$check1==TRUE){
      req(input$select_var3)
      req(input$select_var2)
      req(input$select_var4)
      df_sel1 <- df1()%>%
        mutate(new_var=as.numeric(as.numeric(gsub(",","",df1()[[input$select_var3]]))))%>%
        group_by_(input$select_var4, input$select_var2)%>%
        summarize(sum1=sum(new_var, na.rm= TRUE))%>%
        arrange(desc(sum1))
    }
    else{
      req(input$select_var3)
      req(input$select_var2)  
      df_sel1 <- df1()%>%
        mutate(new_var=as.numeric(as.numeric(gsub(",","",df1()[[input$select_var3]]))))%>%
        group_by_(input$select_var2)%>%
        summarize(sum1=sum(new_var, na.rm= TRUE))%>%
        arrange(desc(sum1))
    }
  })
  
  output$button5 <- downloadHandler(
    filename= function(){
      paste(input$filename4, "csv", sep=".")
    },
    content= function(file){
      write.csv(df_sel1(),file)
    }
  )
  
  output$plot1 <- renderPlot({
    if(input$check1==TRUE){
      ggplot(data=df_sel1(), aes_string(x=input$select_var4, y=df_sel1()$sum1, col=input$select_var2))+
        geom_line()+
        ggtitle(input$title1)+
        xlab(input$xlab1)+
        ylab(input$ylab1)+
        theme
    }
    else{
      ggplot(data=df_sel1(), aes_string(x=input$select_var2, y=df_sel1()$sum1))+
        geom_col(fill="#009999")+
        ggtitle(input$title1)+
        xlab(input$xlab1)+
        ylab(input$ylab1)+
        theme
    }
  })

  output$table1 <- DT::renderDataTable(
    df_sel1()
  )
  ##Total Count Section------------
  df2 <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath,
             header = TRUE,
             stringsAsFactors = FALSE,
             strip.white = TRUE)  
  })
  output$checkbox1 <- renderUI({
    checkboxGroupInput(inputId = "select_var", 
                       label = "Select variables", 
                       choices = names(df2()))})
  
  output$checkbox2 <- renderUI({
    if(input$check==TRUE){
      checkboxGroupInput(inputId= "select_var1",
                         label="Select Time Variable",
                         choices= names(df2()))}
    else{return()}
  })
  df_sel2 <- reactive({
    if(input$check==TRUE){
      req(input$select_var)
      req(input$select_var1)
      x <- df2() %>%
        #select(input$select_var)%>%
        group_by_(input$select_var)%>%
        summarize(sum1=n())%>%
        arrange(desc(sum1))%>%
        filter(dense_rank(-sum1)<11)
      c <- x$input$select_var
      df_sel2 <- df2()%>%
        mutate_(date=as.Date.character(df2()[ ,input$select_var1]))%>%
        group_by_(date, input$select_var)%>%
        summarize(sum1=n())
    }
    else{
      req(input$select_var)
      df_sel2 <- df2() %>%
        #select(input$select_var)%>%
        group_by_(input$select_var)%>%
        summarize(sum1=n())%>%
        arrange(desc(sum1))%>%
        filter(dense_rank(-sum1)<11)
    }
  })
  
  output$button3 <- downloadHandler(
    filename= function(){
      paste(input$filename2, "csv", sep=".")
    },
    content= function(file){
      write.csv(df_sel2(),file)
    }
  )
  
  output$plot <- renderPlot({
    if(input$check==TRUE){
      ggplot(data=df_sel2(), aes_string(x=df_sel2()$month, y=df_sel2()$sum1, col=input$select_var))+
        geom_line()+
        ggtitle(input$title)+
        xlab(input$xlab)+
        ylab(input$ylab)+
        theme
    }
    else{
      ggplot(data=df_sel2(), aes_string(x=input$select_var, y=df_sel2()$sum1))+
        geom_col(fill="#009999")+
        ggtitle(input$title)+
        xlab(input$xlab)+
        ylab(input$ylab)+
        theme
    }
  })
  
  
  output$table <- DT::renderDataTable(
    df_sel2()
  )
}

# Create Shiny app ----
shinyApp(ui, server)
runApp(launch.browser = TRUE)
rlang::last_error()


install.packages('rsconnect')
rsconnect::setAccountInfo(name='iz123',
                          token='219D99448354FCF82D170BC07374E2C3',
                          secret='ZNzdtfsUk7WNN3ge90j4PXfXkBDS8J/kUgaPohGi')
options(rsconnect.max.bundle.size=10000000000)
options(shiny.maxRequestSize=10*1024^10) 

