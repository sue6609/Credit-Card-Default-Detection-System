library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(caret)
library(ggplot2)
library(pROC)
library(DT)
library(reshape2)
library(dplyr)


# 確定app路徑
setwd("C:/Users/f9006/OneDrive/巨量/app")

# 檢查是否加載過檔案
source('bigdata/prog_clean.R')
source('bigdata/var_type_select.R')
source('bigdata/mutifo_fun.R')
source('bigdata/feat_select.R')
source('bigdata/test_clean.R')
source('bigdata/pred_test_data.R')

#####################################################################  

target_1_num_data <- subset(num_vars, TARGET == 1)
target_0_num_data <- subset(num_vars, TARGET == 0)
factor_vars <- select_variables(filter_data_loan, type = "factor")
target_1_fac_data <- subset(factor_vars, TARGET == 1)
target_0_fac_data <- subset(factor_vars, TARGET == 0)

cutdata13 = data.frame(VAR = c("精確率", "召回率", "F1-score", "真陽性率", "準確率"), 
                     Value = c(0.47,0.24,0.32,0.47,0.77))
cutdata14 = data.frame(VAR = c("精確率", "召回率", "F1-score", "真陽性率", "準確率"), 
                     Value = c(0.43,0.25,0.32,0.43,0.80))

################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "信用卡違約偵測系統"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("首頁", tabName = "home", icon = icon("home")),
      menuItem("全體用戶", tabName = "all_users", icon = icon("users"),
               menuSubItem("數據摘要", tabName = "sub_item1",icon = icon("dashboard")),
               menuSubItem("風險分析", tabName = "sub_item2",icon = icon("line-chart"))),
      menuItem("個人用戶", tabName = "personal_users", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidPage(
         fluidRow(column(12,align ="center",   
          div(
            style = "background-image: url('cover1.jpg'); background-size: 100%; background-position: center; height: 50vh; display: flex; flex-direction: column; justify-content: center; align-items: center;",
            h1("信用卡違約偵測系統",style = "color: white;border-bottom: 2px solid white; padding-bottom: 10px;font-size: 60px;font-weight: bold;") 
            
          ))),tags$hr(),
         fluidRow(column(12,
                     div(style = "font-size: 16px;",
                         p("隨著現代社會的發展，信用卡已成為人們日常生活中不可或缺的支付工具之一，但與此同時，信用卡違約的問題也逐漸浮現︒本儀表板旨在以客戶基本資料與交易資料為基礎，呈現信用卡違約風險因素分析與預測，以下是本儀表板的主要功能介紹："),
                         tags$br(),
                         tags$ol(
                           tags$li("資料視覺化：提供多種資料相關統計圖表，便於使用者了解數據分佈及趨勢，且可以根據不同的參數進行過濾與更新。"),
                           tags$br(),tags$li("違約風險預測：應用模型（邏輯斯迴歸）進行信用卡違約風險預測。並提供評估指標。"),
                           tags$br(),tags$li("個案分析：提供單個客戶的分析報告，包括違約風險評估等。")
                         ),tags$br(),
                         p("參與名單：林貫原︑許政揚︑周昱宏︑楊廷紳︑易祐辰︑留筠雅"),
                         p("聯絡窗口：留筠雅",a("s711233112@gm.ntpu.edu.tw", href = "mailto:s711233112@gm.ntpu.edu.tw")),
                         p("有任何問題歡迎寄信詢問。")))
         )
        )
      ),
      tabItem(
        tabName = "sub_item1",
        fluidPage(fluidRow(column(12,h3("類別型變數分佈之長條圖", style = "color:#3C3C3C; font-weight: bold;"),tags$div(style = "height: 30px;"))),
          fluidRow(column(3,
                          radioButtons(
                            "target_radio_2", 
                            label = "選擇客戶有無違約:",
                            choices = list("有" = 1, "無" = 2, "全體" = 3),
                            selected = 3),
                          selectInput('x2col', '選擇變數:', names(factor_vars), selected = "NAME_CONTRACT_TYPE")
                          
          ),
          column(9,
                 plotlyOutput("plot_2"),
                 tags$div(style = "height: 30px;")
                 )),
          fluidRow(column(12,h3("數值型變數分佈之直方圖", style = "color:#3C3C3C; font-weight: bold;"),tags$div(style = "height: 30px;"))),
          fluidRow(column(3,
                          radioButtons(
                            "target_radio", 
                            label = "選擇客戶有無違約:",
                            choices = list("有" = 1, "無" = 2, "全體" = 3),
                            selected = 3),
                          selectInput('xcol', '選擇變數:', names(num_vars)[-29], selected = "CNT_CHILDREN")),
                   column(9,                          
                          plotlyOutput("plot_1"),
                          tags$div(style = "height: 30px;"),
                          valueBoxOutput("valuebox1"),
                          valueBoxOutput("valuebox2"),
                          valueBoxOutput("valuebox3")))
          ,        
          hr(),
          fluidRow(
            column(12,h3("交易資料集", style = "color:#3C3C3C; font-weight: bold;"),tags$div(style = "height: 30px;"))
          ),
          fluidRow(
            column(4,
                   selectInput("target",
                               "是否違約:",
                               c("All",
                                 unique(as.character(filter_data_loan$TARGET))))
            ),
            column(4,
                   selectInput("OCCUPATION_TYPE",
                               "職業種類:",
                               c("All",
                                 unique(as.character(filter_data_loan$OCCUPATION_TYPE))))
            ),
            column(4,
                   selectInput("ORGANIZATION_TYPE",
                               "公司類型:",
                               c("All",
                                 unique(as.character(filter_data_loan$ORGANIZATION_TYPE))))
            )
          ),
          DT::dataTableOutput("table"),
          downloadButton("downloadData", "下載")
        )
      ),
      tabItem(
        tabName = "sub_item2",
        fluidPage(fluidRow(
          box(title = "測試資料之 ROC 曲線",plotlyOutput("plot_auc"),height = 550, solidHeader = TRUE, status = "primary"),
          box(title = "變數影響力", height = 550, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("數值型變數",DTOutput("table_influence_1") ),
                tabPanel("類別型變數",DTOutput("table_influence_2") )
              ))
        ),
        
        fluidRow(
          # box(
          #   height = 200,
          #   title = "平均風險", width = 4, solidHeader = TRUE, status = "info",
          #   "Box content"
          # ),
          box(
            height = 350,
            title = "預測指標", width = 6, solidHeader = TRUE, status = "info",
            tabsetPanel(
              tabPanel("切點：0.13", div( tableOutput("predict_index_13"),style = "font-size:120%"))
              # ,tabPanel("切點：0.14", div( tableOutput("predict_index_14"),style = "font-size:120%"))
            )
          ),
          box(
            height = 500,
            title = "混淆矩陣", width = 6, solidHeader = TRUE, status = "info",
            tabsetPanel(
              tabPanel("切點：0.13",plotOutput("plot_confusion_matrix_13"))
              # ,tabPanel("切點：0.14",plotOutput("plot_confusion_matrix_14") )
            )
          )
        )

        )
      ),
      tabItem(
        tabName = "personal_users",
        fluidPage(p("這個頁面用於收集分析所需資料，請輸入您的回答以便用於預測信用卡違約風險，感謝您。"),
            hr(),      
            fluidRow(
              column(4, numericInput("q1", "外部數據來源2的正規化分數",0,min=0)),
              column(4, numericInput("q2", "外部數據來源3的正規化分數",0)),
              column(4, numericInput("q3", "客戶年齡",20, min = 20))),
            fluidRow(
              column(4, numericInput("q4", "申請貸款前多少年換過手機數",0, min = 0)),
              column(4, numericInput("q5", "申請貸款前多少年變動身分證明文件",0,min = 0)),
              column(4, selectInput("q6","職業種類:", unique(as.character(filter_data_loan$OCCUPATION_TYPE)),
                                    selected = "others"
                                    ))),
            fluidRow(
              column(4, selectInput("q7","公司類型:", unique(as.character(filter_data_loan$ORGANIZATION_TYPE)),
                                    selected = "Business Entity")),
              column(4, selectInput("q8","收入來源:", unique(as.character(filter_data_loan$NAME_INCOME_TYPE)),
                                           selected = "Working")),
              column(4,actionButton("go", "提交！",style = "margin-top: 25px;"))
              ),
            #actionButton("go", "提交！"),
            hr(),
            fluidRow(
              column(6, offset = 3, plotOutput("plot_personal"))
            ) 
            
          
        )
      )
    )
  )
)

############################################################################################


server <- function(input, output) {
  
  selectedData_all <- reactive({
    num_vars[ , input$xcol]
  })
  selectedData_1 <- reactive({
     target_1_num_data[ , input$xcol]
  })
  selectedData_0 <- reactive({
     target_0_num_data[ , input$xcol]
  })
  selectedData_all_fac <- reactive({
    factor_vars[ , input$x2col]
  })
  selectedData_1_fac <- reactive({
    target_1_fac_data[ , input$x2col]
  })
  selectedData_0_fac <- reactive({
    target_0_fac_data[ , input$x2col]
  })
# --------------------------------------------------------------------  
  output$plot_1 <- renderPlotly({
    
  if(input$target_radio == 3) {
  ggplot(
      data =num_vars,
      aes(x = selectedData_all(), fill = TARGET)
    ) + 
      geom_histogram(bins = 25, alpha = 0.8, width = 0.5, color = 'white') +
      labs(
        x = paste("Amount/ Number of", input$xcol),
        y = "Count"
      ) +
      scale_fill_manual(
        values = c("#44af69", "#FF8040"),
        labels = c("No", "Yes"),
        name = "Target"
      ) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 12)
      )+
      scale_x_continuous(lab = scales::comma)+ theme_minimal()
  
 
    } else if(input$target_radio==1){
      
      ggplot(
        data = target_1_num_data,
        aes(
         x = selectedData_1()
        )
      )  +
        geom_histogram(bins = 15, alpha = 0.8, width = 0.5, color = "white", fill = "#FF8040") +
        labs(
          x = paste("Amount/ Number of", input$xcol),
          y = "Count"
        ) +
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          title = element_text(size = 12)
        )+
        scale_x_continuous(lab = scales::comma)+ theme_minimal()
    } else {
      
      ggplot(
        data = target_0_num_data,
        aes(
          x = selectedData_0()
        )
      )  +
        geom_histogram(bins = 20, alpha = 0.8, width = 0.5, color = "white", fill = "#44af69") +
        labs(
          x = paste("Amount/ Number of", input$xcol),
          y = "Count"
        )+
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          title = element_text(size = 12)
        )+
        scale_x_continuous(lab = scales::comma)+ theme_minimal()
    }
  })
#--------------------------------------------------------------------  
 output$plot_2 <- renderPlotly({
  if(input$target_radio_2 == 3) {
    ggplot(
      data = factor_vars,
      aes(x = selectedData_all_fac(), fill = TARGET)
    ) +
      geom_bar(position = "dodge", alpha = 0.8, width = 0.5, color = "white") +
      theme_minimal()+
      labs(
        x = input$x2col,
        y = "Count") +
      scale_fill_manual(
        values = c("#44af69", "#FF8040"),
        labels = c("No", "Yes"),
        name = "Target"
      )+
      theme(
        legend.position = "top",
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      ) +
      scale_y_continuous(lab = scales::comma)
     
  } else if(input$target_radio_2 == 1){
    ggplot(
      data = target_1_fac_data,
      aes(x = selectedData_1_fac())
    ) +
      geom_bar(position = "dodge", alpha = 0.8, width = 0.5, color = "white", fill = "#FF8040" ) +
      theme_minimal()+
      labs(
        x = input$x2col,
        y = "Count") +
      theme(
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      ) +
      scale_y_continuous(lab = scales::comma)
     
  }else{
    ggplot(
      data = target_0_data,
      aes(x = selectedData_0_fac())
    ) +
      geom_bar(position = "dodge", alpha = 0.8, width = 0.5, color = "white", fill = "#44af69" ) +
      theme_minimal() +
      labs(
        x = input$x2col,
        y = "Count") +
      theme(
        
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      ) +
      scale_y_continuous(lab = scales::comma)
        
  }
  })
#-----------------------------------------------------------------------------------------------------------------

output$valuebox1 <- renderValueBox({
     if(input$target_radio == 3) {
       valueBox(
         value = format(round(median(selectedData_all()), 2), big.mark = ","),
         subtitle = "中位數",
         color = "navy"
       )
     }else if (input$target_radio == 1){
       valueBox(
         value = format(round(median(selectedData_1()), 2), big.mark = ","),
         subtitle = "中位數",
         color ="navy"
       )}else{
         valueBox(
           value = format(round(median(selectedData_0()), 2), big.mark = ","),
           subtitle = "中位數",
           color = "navy"
         )
       }

  })
  output$valuebox2 <- renderValueBox({
    if(input$target_radio == 3) {
      valueBox(
        value = format(round(mean(selectedData_all()), 2), big.mark = ","),
        subtitle = "平均數",
        color = "blue"
      )
    }else if (input$target_radio == 1){
      valueBox(
        value = format(round(mean(selectedData_1()), 2), big.mark = ","),
        subtitle = "平均數",
        color = "blue"
      )}else{
        valueBox(
          value = format(round(mean(selectedData_0()), 2), big.mark = ","),
          subtitle = "平均數",
          color = "blue"
        )
      }
  })
  output$valuebox3 <- renderValueBox({
    if(input$target_radio == 3) {
      valueBox(
        value = format(round(sd(selectedData_all()), 2), big.mark = ","),
        subtitle = "標準差",
        color = "light-blue"
      )
    }else if (input$target_radio == 1){
      valueBox(
        value = format(round(sd(selectedData_1()), 2), big.mark = ","),
        subtitle = "標準差",
        color = "light-blue"
      )}else{
        valueBox(
          value = format(round(sd(selectedData_0()), 2), big.mark = ","),
          subtitle = "標準差",
          color = "light-blue"
        )
      }
  })
  
  output$plot_auc <- renderPlotly({
    roc_obj <- roc(filter_data_loan_test[,'TARGET'], pre_respon, levels=c(0, 1), direction="<")
    
    # Create a data frame for plotly
    roc_data <- data.frame(
      tpr = rev(roc_obj$sensitivities) * 100, # True Positive Rate
      fpr = rev(roc_obj$specificities) * 100, # False Positive Rate
      thresholds = rev(roc_obj$thresholds)
    )
    
    # Calculate AUC
    auc_value <- auc(roc_obj)
    
    # Plot ROC curve using plotly
    plot_ly(data = roc_data, x = ~100 - fpr, y = ~tpr, type = 'scatter', mode = 'lines', fill = 'tozeroy', fillcolor = 'rgba(128,128,128,0.2)', line = list(color = 'blue')) %>%
      layout(
             xaxis = list(title = "False Positive Rate"),
             yaxis = list(title = "True Positive Rate"),
             annotations = list(
               x = 60,
               y = 20,
               text = paste("AUC =", round(auc_value, 2)),
               showarrow = FALSE,
               font = list(size = 20) 
             ),
             shapes = list(
               list(type = "line",
                    x0 = 0, y0 = 0,
                    x1 = 100, y1 = 100,
                    line = list(dash = 'dash', color = 'red'))
             ))
  })
  
  output$table_influence_1 <-renderDT(
   cor_test_for_csv1, options = list(lengthChange = FALSE, pageLength = 8)
  )
  output$table_influence_2 <-renderDT(
    mutual_information, options = list(lengthChange = FALSE, pageLength = 8)
  )
  
  output$table <- DT::renderDataTable(
    DT::datatable({
      data = filter_data_loan
      if (input$target != "All") {
       data<- data[filter_data_loan$TARGET == input$target,]
      }
      if (input$OCCUPATION_TYPE != "All") {
        data <- data[data$OCCUPATION_TYPE == input$OCCUPATION_TYPE,]
      }
      if (input$ORGANIZATION_TYPE != "All") {
        data <- data[data$ORGANIZATION_TYPE == input$ORGANIZATION_TYPE,]
      }
      data
    }, options = list(scrollX = TRUE))
  )
  
  output$downloadData <- downloadHandler(
    filename = "creditcard_data.csv",
    content = function(file) {
      write.csv(filter_data_loan, file, row.names = FALSE)
    }
  )

  
  output$predict_index_13 <-   renderTable(
   cutdata13,width = "90%",height = "90%",align = "c"
    
  )
  output$predict_index_14 <-   renderTable(
    cutdata14,width = "90%",height = "90%",align = "c"
    
  )
  
  output$plot_confusion_matrix_13 <- renderPlot({
    
    predicted <- factor(ifelse(pre_respon > 0.13, 1, 0), levels = c("0", "1"))
    
    # 創建包含所有可能類別的空混淆矩陣
    full_confusion_matrix <- table(factor(filter_data_loan_test$TARGET, levels = c("0", "1")), predicted)
    
    # 創建混淆矩陣（只包含實際有出現的類別）
    confusion_matrix <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("0", "1"), c("0", "1")))
    confusion_matrix[rownames(full_confusion_matrix), colnames(full_confusion_matrix)] <- full_confusion_matrix
    
    #換成列百分比
   # confusion_matrix <- round(prop.table(confusion_matrix, 1),2)
    
    #換成長格式
    confusion_matrix_long <- melt(confusion_matrix)
    #轉文字
    confusion_matrix_long$Var2 <- as.character(confusion_matrix_long$Var2)
    confusion_matrix_long$Var1 <- as.character(confusion_matrix_long$Var1)
    
    ggplot(data = confusion_matrix_long, aes(x = Var2, y = Var1, fill = value)) +
      geom_tile() +
      geom_text(aes(label = value), vjust = 1,size = 10) +
      scale_fill_gradient(low = "#D2E9FF", high = "#005AB5") +
      theme_minimal() +
      labs( x = "Predicted", y = "True") +
      theme(text = element_text(size = 18))
  })
  output$plot_confusion_matrix_14 <- renderPlot({
    
    predicted <- factor(ifelse(pre_respon > 0.14, 1, 0), levels = c("0", "1"))
    
    # 創建包含所有可能類別的空混淆矩陣
    full_confusion_matrix <- table(factor(filter_data_loan_test$TARGET, levels = c("0", "1")), predicted)
    
    # 創建混淆矩陣（只包含實際有出現的類別）
    confusion_matrix <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("0", "1"), c("0", "1")))
    confusion_matrix[rownames(full_confusion_matrix), colnames(full_confusion_matrix)] <- full_confusion_matrix
    
    #換成列百分比
    #confusion_matrix <- round(prop.table(confusion_matrix, 1),2)
    
    #換成長格式
    confusion_matrix_long <- melt(confusion_matrix)
    #轉文字
    confusion_matrix_long$Var2 <- as.character(confusion_matrix_long$Var2)
    confusion_matrix_long$Var1 <- as.character(confusion_matrix_long$Var1)
    
    ggplot(data = confusion_matrix_long, aes(x = Var2, y = Var1, fill = value)) +
      geom_tile() +
      geom_text(aes(label = value), vjust = 1,size = 10) +
      scale_fill_gradient(low = "#D2E9FF", high = "#005AB5") +
      theme_minimal() +
      labs( x = "Predicted", y = "True") +
      theme(text = element_text(size = 18))
  })
  
  vals <- eventReactive(input$go, {
    
  model = coef(glm.fit)
  OCCUPATION_TYPE_coef = model[grep("OCCUPATION_TYPE", names(model))]
  names_OCCUPATION = sub("^OCCUPATION_TYPE", "", names(OCCUPATION_TYPE_coef) ) 
  names(OCCUPATION_TYPE_coef) = names_OCCUPATION
  A = 0
  names(A) = "Accountants"
  OCCUPATION_TYPE_coef = c(OCCUPATION_TYPE_coef, A)
  
  ORGANIZATION_TYPE_coef = model[grep("ORGANIZATION_TYPE", names(model))]
  names_ORGANIZATION = sub("^ORGANIZATION_TYPE", "", names(ORGANIZATION_TYPE_coef))
  names(ORGANIZATION_TYPE_coef) = names_ORGANIZATION
  B = 0
  names(B) = "Advertising"
  ORGANIZATION_TYPE_coef = c(ORGANIZATION_TYPE_coef, B)
  
  NAME_INCOME_TYPE_coef = model[grep("NAME_INCOME_TYPE", names(model))]
  names_NAME_INCOME_TYPE = sub("^NAME_INCOME_TYPE", "", names(NAME_INCOME_TYPE_coef))
  names(NAME_INCOME_TYPE_coef) = names_NAME_INCOME_TYPE
  
  C = 0
  names(C) = "Businessman"
  NAME_INCOME_TYPE_coef = c(NAME_INCOME_TYPE_coef,C )
  
  x = -1 * ( model[1] +
             model["EXT_SOURCE_2"]*input$q1 + 
             model["EXT_SOURCE_3"]*input$q2 + 
             model["YEARS_BIRTH"]*input$q3 + 
             model["YEARS_LAST_PHONE_CHANGE"]*input$q4 + 
             model["YEARS_ID_PUBLISH"]*input$q5 +
             OCCUPATION_TYPE_coef[input$q6] +
             ORGANIZATION_TYPE_coef[input$q7]+
             NAME_INCOME_TYPE_coef[input$q8]
               )
  p = 1/(1+exp(x))
  
  })
 
  
 output$plot_personal <- renderPlot({

data = data.frame(percent = c(1-vals(),vals()), level = c("無違約", "違約"))
risk_message <- ifelse(data$percent[2] > 0.13, "High Risk", ifelse(0.13 >= data$percent[2] & data$percent[2] > 0.1, "Medium Risk", "Low Risk"))
risk_color = ifelse(data$percent[2] > 0.13, "#FF6347",ifelse(0.66>=data$percent[2] & data$percent[2] > 0.1, "#FFE153", "#44af69"))
#畫一張圓餅圖
ggplot(data, aes(x="", y=percent, fill=level))+
  geom_bar(stat="identity",alpha=0.8)+
  expand_limits(x=c(-2,1))+
  coord_polar("y", start=0)+
  ggtitle("Customer Credit Card Default Risk")+
  theme_minimal()+
  scale_fill_manual(values =c("無違約" = "#F0F0F0","違約" = risk_color ))+
  theme_void() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 22, family = "Arial"))+
  annotate("text", x = 0, y = 0, label = paste0( round(data$percent[2] * 100, 2), "%"), size = 18, hjust = 0.45, vjust = 3, color = "#666666",fontface = "bold")+
  annotate("text", x = 0, y = 1, label = risk_message, size = 8, hjust = 0.45, vjust = 2, color = risk_color,fontface = "bold")
 })

}
#----------------------------------------------------------------------------------------------------


# Run the app ----
shinyApp(ui = ui, server = server)
