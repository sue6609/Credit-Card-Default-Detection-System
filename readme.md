信用卡違約偵測系統

這個Shiny網頁應用程式旨在使用各種統計技術來分析和偵測信用卡違約。該應用包括數據清理、變數選擇、模型訓練和評估功能。

## 目錄

1. [安裝](#安裝)
2. [使用](#使用)
3. [文件結構](#文件結構)
4. [必要套件](#必要套件)
5. [功能](#功能)
6. [致謝](#致謝)

## 安裝

1. 確保您已安裝R和RStudio。
2. 通過在R中運行以下命令來安裝所需的套件：
    ```R
    install.packages(c("shiny", "shinydashboard", "shinythemes", "plotly", "caret", "ggplot2", "pROC", "DT", "reshape2", "dplyr"))
    ```

## 使用

1. 將工作目錄設置為您的應用程式位置：
    ```R
    setwd("/home/user/documents/file")
    ```
2. 加載必要的R程式檔：
    ```R
    source('bigdata/prog_clean.R')
    source('bigdata/var_type_select.R')
    source('bigdata/mutifo_fun.R')
    source('bigdata/feat_select.R')
    source('bigdata/test_clean.R')
    source('bigdata/pred_test_data.R')
    ```
3. 運行Shiny應用程式：
    ```R
    runApp('app_final.R')
    ```

## 文件結構

- app_final.R：Shiny應用程式的主要R程式檔。
- bigdata/：包含數據處理和模型函數的支持R腳本的目錄。
- prog_clean.R：訓練集資料清洗。
- var_type_select.R：挑選數值型或類別型變數，進行抽樣。
- mutifo_fun.R：針對類別型變數對目標變數的影響力。
- feat_select.R：針對數值型變數對目標變數的影響力。
- test_clean.R：測試集資料清洗。
- pred_test_data.R：生成模型並預測。

## 必要套件

此應用程式依賴以下R套件：
- shiny =1.8.1.1
- shinydashboard =0.7.2
- shinythemes =1.2.0
- plotly =4.10.4
- caret =6.0-94
- ggplot2 =3.5.0
- pROC =1.18.5
- DT =0.33
- reshape2 =1.4.4
- dplyr =1.1.4
- R version 4.3.3 (2024-02-29 ucrt)


## 功能

- **首頁**：應用程式概述。
- **全體用戶**：
  - **數據摘要**：數據集的摘要統計和視覺化。
  - **風險分析**：對導致信用卡違約的風險因素的分析。
- **個人用戶**：個別用戶的詳細分析。

## 致謝

衷心感謝巨量資料分析的教授指導與組員們的貢獻!!
