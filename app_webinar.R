#### R-Consortium Webinar###########################################
#### Participation Risk#############################################
#### Simon Aigner, Goran Lovric ####################################
#### Version 1.0####################################################
####################################################################

library(shinydashboard)
library(DBI)
library(magrittr)
library(ggplot2)
library(zoo)
library(shiny)
library(scales)
library(openxlsx)
library(shinyBS)
library(shinyjs)
library(lubridate)
library(dplyr)
library(DT)
library(tools)
library(writexl)
library(waiter)
library(sqldf)
library(RODBC)

ui <- dashboardPage(skin = "purple",
                    
  dashboardHeader(title = "App v1",tags$li(a(img(src = 'RC_Logo.png',
                                                                     title = "Participation risk", height = "35px"),
                                                                 style = "padding-top:10px; padding-bottom: 8px;"),
                                                               class = "dropdown")
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "dashboard"), 
      uiOutput("usermanual")
      
    )
    
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                background-color: #f0f8ff;
                              }'))),
    
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              fluidRow(
                use_waitress("red"),
                column(width = 4,offset=0.5,style='margin-bottom:30px;border:1px solid;',
                p(strong("1. Inputdata")),
                fluidRow(column (width = 3, 
                                 conditionalPanel(
                                   condition = "input.modus == 'Month end'",
                                 dateInput("date", "Reporting date",value = as.Date(as.yearmon(Sys.Date())) - 1)),
                                 conditionalPanel(
                                   condition = "input.modus == 'Simulation'",
                                   dateInput("date", "Reporting date",value = Sys.Date()))
                                 
                                 ),
                column(width = 6, fileInput("file1", "Upload input file*", buttonLabel = "Search...",placeholder = "")) ),
        
                fluidRow(column(width=4, selectInput("modus", "Calculation mode*",c("Month end","Simulation"))),
                         column(width = 4,sliderInput(inputId ="Sim_num","Number of Simulations" ,value = 300, min=300, max=30000))
                ),
                fluidRow( 
                         column(width = 4, sliderInput(inputId ="KI_num","Confidence intervall", min=0.95, max=0.999, value = 0.999))),
             
               
                
                fluidRow(
                  column(width=4,actionButton("calculate", "Calculate")),
                  column(width=4, actionButton("reset", "Reset values")),
                  column(width=4, actionButton("dbSave", "Save values"))
                )
                ),
                column(width = 8,
                       plotOutput("plot1")
                )
              ), 
              fluidRow(
                valueBoxOutput("warning3"), 
                valueBoxOutput("MC"), 
                valueBoxOutput("out")),
              fluidRow(
                valueBoxOutput("Box1_div"), 
                valueBoxOutput("Box2_div"), 
                valueBoxOutput("Box3_div")),
              fluidRow(
            br(),
            br(),
            br(),
            br()
              ),
              
              fluidRow(
                              style='margin-bottom:30px;border:1px solid;',
                fluidRow(column(width = 4,offset=0.5,
                       p(strong("2. Participation risk - Detailed view"))
                       
                ))
            
                
              ),
            
            
            fluidRow(DT::dataTableOutput("cases"),
                     downloadButton("download_cases"),
              br(),
              br()
            )
    )
  )
)
)


server <- function(input, output, session) {

  rv <- reactiveValues(add_button = 0, add_button_col = 0, cases=NULL, cases2=NULL, cases3=NULL)

  waitress <- Waitress$new("nav", theme = "overlay", min = 0, max = 10)
  
 
  query_modal <- modalDialog(
    title = "R-Consortium Webinar Dec 12 2024",
    p("This application has been developed for the purpose of the above mentioned webinar. Fields marked with an asterisk (*) are mandatory inputs in order to initiate the model embedded in the application.", tags$br(),tags$br(), "For additional information please contact:", tags$br(), tags$br(),tags$b("Simon Aigner, MSc"),tags$br(),tags$a(href="mailto:simon.aigner@rlbooe.com", "simon.aigner@rlbooe.com"),tags$br(),tags$b("Goran Lovric, LL.M."),tags$br(),tags$a(href="mailto:goran.lovric@rlbooe.com", "goran.lovric@rlbooe.com")),
    easyClose = T,
    size="m",
    footer = tagList(
      actionButton("run", "Confirm")
    )
  )
  
  query_modal2 <- modalDialog(
    title = "Notification",
    p("Values have been saved in the database"),
    easyClose = T,
    size="m",
    footer = tagList(
  
      actionButton("ok", "Understood and close")
    )
  )
  

  showModal(query_modal)
  
  observeEvent(input$run, {
    removeModal()
    
  })

  suppressWarnings( 
  observeEvent(input$calculate, {
    
    for(i in 1:10){
      waitress$inc(1) 
      Sys.sleep(.5)
    }
   
    file <- input$file1
    
    
    Modell1<-loadWorkbook(file$datapath)
    
    outputtable<-read.xlsx(Modell1,sheet=1)
    outputtable$Date<-input$date
  
  # Simulations
    
    simtable<-as.data.frame(matrix(NA, ncol=input$Sim_num, nrow=nrow(outputtable)))
    delta_CoE<- as.data.frame(matrix(NA, ncol=input$Sim_num, nrow=nrow(outputtable)))
    delta_CF<-as.data.frame(matrix(NA, ncol=input$Sim_num, nrow=nrow(outputtable)))
    
    

    
    
    EV<-as.data.frame(matrix(NA, ncol=input$Sim_num, nrow=nrow(outputtable)))
    for ( i in (1:nrow(simtable))){
      
      rand1<-rnorm(input$Sim_num)
      #Cholesky 
      rand2<-rnorm(input$Sim_num)
      rand3<-rand1* outputtable$korr[i] +rand2*sqrt(1-outputtable$korr[i]^2)

      delta_CoE[i,]<-rand1*outputtable$sigmaCoE[i]+outputtable$muCoE[i]

      delta_CF[i,]<-outputtable$muCF[i]+outputtable$sigmaCF[i]*rand3

      EV[i,]<- (outputtable$Cashflow[i]*(1+delta_CF[i,]))/(outputtable$COE[i]*(1+delta_CoE[i,]))

    }
    
    
    
    outputtable$Value999<-NA
    
    for (i in (1:length(outputtable$Value999))){
    outputtable$Value999[i]<- unlist(ifelse(outputtable$MV[i]>0, quantile(EV[i,], 0.001,na.rm=T),quantile(EV[i,], 0.999,na.rm=T)))
   
    }
    
    outputtable$VaR<-NA
    
    for (i in (1:length(outputtable$VaR))){
    
    outputtable$VaR[i]<-max((unlist(outputtable$Value999[i]))/outputtable$MV[i]-1,-1)
    } 
    
    outputtable$VaR[is.nan(outputtable$VaR)]<-0
    outputtable$Value999_ownshare<-outputtable$MV*(1+outputtable$VaR)*outputtable$Share
    outputtable$MV_ownshare<- outputtable$MV*outputtable$Share
    outputtable$VaR_undiv<-outputtable$MV_ownshare- outputtable$Value999_ownshare
    
### Data can be obtained with Webscraping techniques presented - no excel sheet needed
      CorData<-loadWorkbook("data/Correl_Industries.xlsx")
      CorData_final<-read.xlsx(CorData,sheet=1, colNames = T, rowNames = T)
    
      Total_VaR<-sqrt(t(as.matrix(outputtable$MV_ownshare - outputtable$Value999_ownshare)) %*% as.matrix(CorData_final) %*% (as.matrix((outputtable$MV_ownshare - outputtable$Value999_ownshare))))
      Total_VaR_share<-matrix(NA, nrow(CorData_final) ,1)
      undiv_VaR<-as.matrix(outputtable$MV_ownshare- outputtable$Value999_ownshare)
    
      
      #marginal VaR
      for (i in (1:nrow(Total_VaR_share))){
        undiv_VaR2<-undiv_VaR
        undiv_VaR2[i]<-0
        Total_VaR_share[i,]<-Total_VaR-(sqrt(t(undiv_VaR2)%*%as.matrix(CorData_final) %*%undiv_VaR2))
        
      }
      
      Total_VaR_share<-Total_VaR_share/(sum(Total_VaR_share))*as.numeric(Total_VaR)

      outputtable$VaR_div<-unlist(as.numeric(Total_VaR_share))
     

    ##dashboard
    
    sorted_tab<-outputtable %>% 
      arrange(desc(MV))
    
    cols.dont.want <- c("Internal_ID","sigmaCoE",	"muCoE",	"sigmaCF",	"muCF","korr", "Cashflow", "Share", "MV", "COE", "Peergroup") ##
    sorted_tab_sum<-outputtable[, ! names(outputtable) %in% cols.dont.want, drop = F]
    sorted_tab_sum<-sorted_tab_sum %>% 
      mutate_if(is.numeric, round, digits=3)
    
    
    part_risk_undiv<-sum(sorted_tab$MV_ownshare)-sum(sorted_tab$Value999_ownshare)
    part_risk_div<-sum(sorted_tab$VaR_div)
    total_risk<-sum(sorted_tab$Value999_ownshare)
    part_risk_undiv_pct<-(sum(sorted_tab$MV_ownshare)-sum(sorted_tab$Value999_ownshare))/sum(sorted_tab$MV_ownshare)
    part_risk_div_pct<-sum(sorted_tab$VaR_div) /sum(sorted_tab$MV_ownshare)
    sorted_tab$totalrisk<-sorted_tab$MV_ownshare-sorted_tab$Value999_ownshare
    input_2<-sorted_tab$Name[ which(sorted_tab$totalrisk== max(sorted_tab$totalrisk))]
    input_div<-sorted_tab$Name[ which(sorted_tab$VaR_div== max(sorted_tab$VaR_div))]
    
    rv$cases <- outputtable
    rv$cases2 <- sorted_tab_sum
    
    output$warning3 <- renderValueBox({
      valueBox( 
        value1 <-  tags$p(percent(part_risk_undiv_pct,accuracy = 0.1), style = "font-size: 66%;"),
        subtitle = "Participation risk - non-diversified", 
        icon = icon("percent"),
        color = "aqua"
      )
    })
    
    
    output$Box1_div <- renderValueBox({
      valueBox( 
        value1 <-  tags$p(percent(part_risk_div_pct,accuracy = 0.1), style = "font-size: 66%;"),
        subtitle = "Participation risk - diversified", 
        icon = icon("percent"),
        color = "aqua"
      )
    })
    
    
    output$MC <- renderValueBox({
      valueBox( 
        value1 <-  tags$p(input_2, style = "font-size: 66%;"),
        subtitle = "Largest single contribution - non-diversified", 
        icon = icon("fire"),
        color = "red"
      )
    })
    
    
    output$Box2_div <- renderValueBox({
      valueBox( 
        value1 <-  tags$p(input_div, style = "font-size: 66%;"),
        subtitle = "Largest single contribution - diversified", 
        icon = icon("fire"),
        color = "red"
      )
    })
    
    output$out <- renderValueBox({
      valueBox( 
        value1 <-  tags$p(format(part_risk_undiv*1000,big.mark = ",",scientific=FALSE), style = "font-size: 66%;"),
        subtitle = "Total VaR in EUR - non-diversified", 
        icon = icon("money"),
        color = "yellow"
      )
    })
    
    
    output$Box3_div <- renderValueBox({
      valueBox( 
        value1 <-  tags$p(format(part_risk_div*1000,big.mark = ",",scientific=FALSE), style = "font-size: 66%;"),
        subtitle = "Total VaR in EUR - diversified", 
        icon = icon("money"),
        color = "yellow"
      )
    })
  
      output$plot1 <- renderPlot({
        
        balance <- data.frame(desc = c("Participation in EUR",sorted_tab$Name[1],
                                       sorted_tab$Name[2], sorted_tab$Name[3], sorted_tab$Name[4],sorted_tab$Name[5],
                                        "Rest", "Exposure excl. Participation risk non-div."), amount = c(sum(sorted_tab$MV_ownshare), -(sorted_tab$MV_ownshare[1]-sorted_tab$Value999_ownshare[1]) ,
                                                                                                                                                   -(sorted_tab$MV_ownshare[2]-sorted_tab$Value999_ownshare[2]), -(sorted_tab$MV_ownshare[3]-sorted_tab$Value999_ownshare[3]), -(sorted_tab$MV_ownshare[4]-sorted_tab$Value999_ownshare[4]), -(sorted_tab$MV_ownshare[5]-sorted_tab$Value999_ownshare[5]), -sum(sorted_tab$MV_ownshare[6:length(sorted_tab$MV_ownshare)]-sorted_tab$Value999_ownshare[6:length(sorted_tab$MV_ownshare)]),total_risk))
        balance$desc <- factor(balance$desc, levels = balance$desc)
        balance$id <- seq_along(balance$amount)
        balance$Summary <- ifelse(balance$amount > 0, "Participation",
                               "Participation risk")
        balance[balance$desc %in% c("Share in EUR", "Participation risk non-div."),
                "Type"] <- "net"
        
        balance$end <- cumsum(balance$amount)
        balance$end <- c(head(balance$end, -1), 0)
        balance$start <- c(0, head(balance$end, -1))
        
        ggplot(balance, aes(desc, fill = Summary),colour="lightblue") + geom_rect(aes(x = desc,
                                                                                   xmin = id - 0.45, xmax = id + 0.45, ymin = end,
                                                                                   ymax = start)) +labs(x = "Participation", y="Amount in EUR", title = paste( "Participation risk:",round(part_risk_undiv,2), "Million EUR and", round(part_risk_undiv_pct*100,1), "percent", sep=" ")) + geom_text(subset = (balance$Summary == "Participation"), aes(id,
                                                                                                                                                                                                 end, label = comma(amount)), vjust = 1, size = 3)+
          theme(
            panel.background = element_rect(fill = "aliceblue") # bg of the panel
            , plot.background = element_rect(fill = "aliceblue") # bg of the plot
            , panel.grid.major = element_blank() # get rid of major grid
            , panel.grid.minor = element_blank() # get rid of minor grid
            , legend.background = element_rect(fill = "aliceblue") # get rid of legend bg
            , legend.box.background = element_rect(fill = "aliceblue") # get rid of legend panel bg
          )
        
      })
     
      waitress$close()                                 
  })
  )

  observeEvent(input$reset,{session$reload() })
  
  
  
  observeEvent(input$dbSave,{

    outputtable<-rv$cases 
    
    require("RSQLite")
    # Set up database
    drv <- dbDriver("SQLite")

    con <- dbConnect(drv, dbname = "data/PartRiskDatabase.db")
    
    data_available <- dbGetQuery(con,paste("select * FROM PartRiskReport where Date = ",as.numeric(outputtable$Date[1])))
    if (dim(data_available)[1]==0){
  # dbWriteTable(con, "PartRiskReport", outputtable, row.names=NULL)
    dbAppendTable(con, "PartRiskReport", outputtable, row.names=NULL)
  }
  else{
    

      dbSendStatement(con,paste("delete FROM PartRiskReport where Date = ",as.numeric(outputtable$Date[1])))

      dbAppendTable(con, "PartRiskReport", outputtable, row.names=NULL)

  }

    dbDisconnect(con)

    showModal(query_modal2)
    observeEvent(input$ok, {
      removeModal()
 
  })
  
  })

  output$download_cases <-  downloadHandler(
    filename = function() { paste0("Participationriskmodel_",format(Sys.time(), "%Y%m%d_%H%M%S"),".xlsx")},
    content = function(file) {write_xlsx(list(Summary=rv$cases ), path = file)}
  )
  

  output$cases <- DT::renderDataTable({
    
  cases<-rv$cases2
 
  datatable(cases,options=list(ordering = TRUE, order=list(c(7, 'desc'), c(6, 'desc'))))
  })
  

  output$usermanual <- renderUI({
   
      
      sidebarUserPanel("",
                       
                       
                       subtitle = a(icon("book"), "User Manual", href="",target="_blank")
      )
    
  })

 }

shinyApp(ui, server)
