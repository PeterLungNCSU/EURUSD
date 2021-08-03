#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(caret)


ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "EUR/USD Exchange Rate Forecast Project"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("about")),
            menuItem("Data", tabName = "data", icon = icon("data")),
            menuItem("Data Exploration", tabName = "eda", icon = icon("eda")),
            menuItem("Modeling", tabName = "model", icon = icon("model"), 
                     menuSubItem("Modeling Info", tabName = "info", icon = icon("info")),
                     menuSubItem("Model Fit", tabName = "fit", icon = icon("fit")),
                     menuSubItem("Predictions", tabName = "pred", icon = icon("pred")))
                )
            ),
    dashboardBody(
        tabItems(
            tabItem("about",
                fluidPage(
                    h1("The Euro/US Dollar Exchange Rate"),
                    #image_read("Exchange Rates.jpeg"),
                    p("The purpose of this app is to model the exchange rate between the Euro currency and the US Dollar. 
                      There are two competing ideas on the ability to model financial instruments - including currency 
                      exchange rates. The first is called the ",strong("Efficient Market Hypothesis"), " which basically 
                      says that long term success cannot be gained from modeling financial instruments. The second idea 
                      is called ",strong("Technical Analysis"), " which purports that certain quantities called indicators 
                      can help determine how likely the price on a financial instrument is to go up or down. In this app, 
                      I plan to explore the merits of technical analysis using historical data on financial markets, 
                      specifically, the EURUSD currency pair exchange rate.", style = "font-size:20px;"),
                    br(),
                    h2("The Efficient Market Hypothesis"),
                    p("Many economists believe that financial markets do not move in predictable ways that someone 
                      could take advantage of to make a significant amount of money. They call this the 'efficient 
                      market hypothesis'. The idea is that because such a large number of people are involved in 
                      trading in that market, and since the stakes are so high, if there was a way to take advantage
                      of some systemic inefficiency, the market would have already done so and that inefficiency
                      would disappear almost immediately. There have been attempts to disprove the efficient market 
                      hypothesis, but doing so empirically has been a difficult task. Even harder, however, is proving 
                      its correctness, as you would have to show empirically that any dataset could not systematcally 
                      demonstrate consistent positive returns.", style = "font-size:20px;"),
                    br(),
                    p("The quandry for proponents of this theory, however, is that hedge funds and investment banks 
                      that make promises of certain returns (that beat the market) is big business and those that study 
                      and work in this field make large amounts of money - including those that profess to use technical 
                      analysis. Quantifying their methods can be tricky, since instinct, experience and other factors that 
                      are hard to measure are often utilized by these individuals.", style = "font-size:20px;"),
                    br(),
                    h2("Technical Analysis"),
                    p("Technical analysis is the study of various measures called 'indicators', which are usually functions 
                      of past observations of the price. These indicators are supposed to measure things like momentum of 
                      the market, whether the market is 'over-bought' or 'over-sold', activity of large banks and other 
                      things. The idea behind technical analysis is that when certain conditions that pertain to these 
                      indicators are met, there is an increased probability that the price will increase or decrease and a 
                      trader can take advantage of this information and make informed trades.", style = "font-size:20px;"),
                    br(),
                    p("Since all of the measures used in technical analysis and quantitative and can be used in a dataset, 
                      it stands to reason that the effectiveness of various technical analysis indicators can be measured 
                      and probabilities of success accurately measured.", style = "font-size:20px;"),
                    br(),
                    h2("The Hypothesis"),
                    p("I am putting forth a null and an alternative hypothesis. The null hypothesis is that technical analysis 
                      as it is written about and taught cannot be used as a long-term money making strategy in financial markets. 
                      The alternative hypothesis is that technical analysis does work and that one could make money utilizing it. 
                      To test these hypotheses, I will be building three types of models using the one-step-ahead change in price 
                      of the EURUSD exchange rate as the response and various autoregressive terms and technical analysis 
                      indicators as predictors. The three models are: 1) a linear regression model, 2) a regression tree, and 3) 
                      a random forest model. The test will hopefully show what returns would look like if one utilized all 
                      available information pertaining to these measures to generate forecasted returns (and act on them).", 
                      style = "font-size:20px;"),
                    br(),
                    h2("Some Preliminaries and Terminology"),
                    br(),
                    h4(strong("Pip")),
                    p("The basic unit of measurement for changes in the exchange rate. FDor EURUSD, this is 0.0001 Euros per 
                      Dollar. For context, with a factor of 10 (see below), risking the full account of $10,000 and winning 
                      one pip would net $10 for EURUSD.", style = "font-size:17px;"),
                    h4(strong("Spread")),
                    p("The spread represents the amount of money charged by the broker who executes a trade for a trader. It 
                      is proportional to the amount traded and is measured in pips (usually about 1.4 - 1.7 pips, but this 
                      varies over the course of the day.", style = "font-size:17px;"),
                    h4(strong("Multiplicative Factor")),
                    p("Brokers allow traders to essentially borrow money up to a certain amout proportional to their account 
                      size. This can range anywhere from 20 times to 100 times the amount of money in the account. This can 
                      be done with low risk because in reality, the exchange rates move so slowly that day traders would not 
                      be making any money on trades, even if they won 'big' because the relative size of big is in fact 
                      pennies on a bet of $100. Allowing this multiplicative factor, however, allows gains that will 
                      actually entice people to make trades (making the brokers more money).", style = "font-size:17px;")
                )
            ),
            tabItem("data",
                    fluidPage(
                        h1("The Euro US Dollar Exchange Rate"),
                        br(),
                        h3("The Data"),
                        p("The data is observed exchange rate prices every three hours starting on February 17th, 2015 to July 23rd, 
                          2021.", style = "font-size:20px;"), 
                        br(),
                        h3("Response"),
                        p("The response, 'y', is defined as the three hour change in price from the Date/Time indicated to 
                          three hours ahead of that time.", style = "font-size:20px;"),
                        br(),
                        h3("Lags of y"),
                        p("There are three autoregressive terms which are 1, 2 and 3 period lagged 
                          values of the response.", style = "font-size:20px;"),
                        br(),
                        h3("Exponential Moving Average"),
                        p("EMA stands for 'exponential moving average' which is a weighted average of recent 
                          prices with higher weights more recently and lower weights further back in time. The smaller the number 
                          associated with EMA, the more weight is on recent observations and the less is on further back ones. Higher 
                          numbers allow more weight further back in time. The EMA subtracts off the current price to make the variable 
                          stationary.", style = "font-size:20px;"),
                        br(),
                        h3("CMO"),
                        p("CMO is an indicator defined as the number of price increases minus the number of price decreases 
                          divided by the number of periods. It ranges from -1 (all decreases) to 1 (all increases).", style = "font-size:20px;"),
                        br(),
                        h3("MACD"),
                        p("MACD measuresrelative differences in EMA between short and long term. It takes the EMA12, subtracts the EMA26, and takes 
                          that quantity minus the exponential moving average of that quantity to get a difference of long and short 
                          term moving averages.", style = "font-size:20px;"),
                        br(),
                        br(),
                        h3("Filtering"),
                        p("Use the check boxes below to select the variables you would like to include in the three models (Linear 
                          Regression, Regression Tree and Random Forest). You can also choose the set of dates to use for running the 
                          model by selecting from the 'Start Date' and 'End Date' calendar widgets below. You can also scroll through the 
                          data to view what it looks like, and download it to a csv.", style = "font-size:20px;"),
                        br(),
                        br(),
                        dateInput("date1", "Start Date", value = "2015-01-29", format = "mm/dd/yy", min = "2015-01-29", max = "2021-07-23"),
                        dateInput("date2", "End Date", value = "2021-07-23", format = "mm/dd/yy", min = "2015-01-29", max = "2021-07-23"),
                        uiOutput("choose_columns"),
                        dataTableOutput("dataset"),
                        br(),
                        downloadButton("downloadData", "Download Data")
                    )
            ),
            tabItem("eda",
                    fluidPage(
                        h1("Exploratory Data Analysis"),
                        p("Use the drop-down menu to select a univariate histogram of the data or a scatterplot which 
                          depicts the average y in a given bucket of the selected predictor variable.", style = 
                          "font-size:20px;"),
                        br(),
                        selectInput("plotType", "Select a plot type:", 
                                    c("Histogram", "Scatterplot")),
                        radioButtons("radioButtons", "Choose the predictor variable:",
                                     list("AR1", "AR2", "AR3", "EMA05", "EMA10", "CMO24", "CMO48","MACD"), inline = TRUE),
                        plotOutput("edaPlot"),
                        br(),
                        downloadButton("downloadPlot", "Download Plot"),
                        br(),
                        br(),
                        h3("Summary Statistics for All Variables"),
                        p("Use the check boxes to show the summary statistics of interest.", style = "font-size:20px;"),
                        br(),
                        checkboxGroupInput("summary", "Select Summary Statistics:",
                                       choiceNames = list("Max", "Q3", "Mean", "Median", "Q1", "Min"),
                                       choiceValues = list("Max.", "3rd Qu.", "Mean", "Median", "1st Qu.", "Min."),
                                       inline = TRUE, selected = c("Max.", "3rd Qu.", "Mean", "Median", "1st Qu.", "Min.")),
                        tableOutput("dataset2")
                        
                        
                    )
            ),
            tabItem("model",
                    h1("Modeling")
            ),
            tabItem("info",
                    h1("Modeling Information"),
                    p("There are three types of models used in this analysis: A linear regression model, a regression 
                      tree model and a random forest model. It is expected that the best fit will likely be the random 
                      forest model since it has a tendency to pick up functional forms very well. I will begin with a 
                      brief discussion of each model.", style = "font-size:20px;"),
                    br(),
                    h3("Linear Regression"),
                    p("Linear Regression models assume a linear relationship between the predictors specified and 
                      the response variable. It fits the model by estimating coefficients on each predictor variable 
                      and an intercept that minimize the sum of the squared distances between the prediction and the 
                      actual observed response value. One can specify nonlinear relationships between predictors and 
                      the response by transforming predictors (square, square root, log, etc.), but this must usually 
                      be done manually.", style = "font-size:20px;"),
                    br(),
                    h3("Regression Tree"),
                    p("Regression trees are models where an algorithm sequentially chooses break points in the data 
                      to create separate estimates for. For instance, the model chooses an x* and has an estimate 
                      for the response when X < x* and another estimate for the response when X > x*. It chooses x* 
                      by finding the point that minimizes the squared residuals. The tree chooses new break points 
                      repeatedly until the chosen parameters call for it to stop.", style = "font-size:20px;"),
                    br(),
                    h3("Random Forest"),
                    p("Bagging is a tree method that uses bootstrapping to select the tree by randomly sampling with 
                      replacement n times from the dataset and then averaging a series of bootstrappedc models to 
                      arrive at an estimate. Random Forest is a form of bagging where the predictor variables are 
                      randomly subsetted and models fit to prevent strong predictors from overpowering less-powerful
                      but still helpful predictors.", style = "font-size:20px;")
            ),
            tabItem("fit",
                    fluidPage(
                        h1("Model Fit"),
                        br(),
                        p("Input the proportion of the sample to be used for training.", style = "font-size:20px;"),
                        br(),
                        sliderInput("trainData", "Proportion of data for training the models",
                                    min = 0.3, max = 0.99, value = 0.75, step = 0.01),
                        br(),
                        actionButton("runButton", "Run"),
                        br(),
                        br(),
                        h3("Linear Regression Model Output"),
                        br(),
                        tableOutput("dataset3"),
                        br(),
                        br(),
                        h3("Regression Tree Model Output"),
                        br(),
                        tableOutput("dataset4"),
                        br(),
                        br(),
                        h3("Random Forest Model Output"),
                        br(),
                        tableOutput("dataset5"),
                        br(),
                        br(),
                        h3("Linear Regression Model Holdout Results"),
                        br(),
                        tableOutput("dataset6"),
                        br(),
                        br(),
                        h3("Regression Tree Model Holdout Results"),
                        br(),
                        tableOutput("dataset7"),
                        br(),
                        br(),
                        h3("Random Forest Model Holdout Results"),
                        br(),
                        tableOutput("dataset8")
                    ) 
            ),
            tabItem("pred",
                    fluidPage(
                        h1("Predictions"),
                        br(),
                        p("Enter your own values for each variable and see what the models predict!"),
                        br(),
                        br(),
                        numericInput("num_AR1", "AR1", 0, min = -0.05, max = 0.05),
                        numericInput("num_AR2", "AR2", 0, min = -0.05, max = 0.05),
                        numericInput("num_AR3", "AR3", 0, min = -0.05, max = 0.05),
                        numericInput("num_EMA05", "EMA05", 0, min = -0.05, max = 0.05),
                        numericInput("num_EMA10", "EMA10", 0, min = -0.05, max = 0.05),
                        numericInput("num_CMO24", "CMO24", 0, min = -1, max = 1),
                        numericInput("num_CMO48", "CMO48", 0, min = -1, max = 1),
                        numericInput("num_MACD", "MACD", 0, min = -0.005, max = 0.005),
                        br(),
                        br(),
                        h4("Linear Regression Prediction"),
                        textOutput("predLM"),
                        br(),
                        h4("Regression Tree Prediction"),
                        textOutput("predTR"),
                        br(),
                        h4("Random Forest Prediction"),
                        textOutput("predRF")
                    ) 
            )

        )
    )
)


server <- function(input, output, session) {
    
    
    dataset <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/EURUSD_data.csv")
    
    varSet <- list("AR1", "AR2", "AR3", "EMA05", "EMA10", "CMO24", "CMO48","MACD")
    avgPlot_y = data.frame(index = c(1:25))
    avgPlot_x = data.frame(index = c(1:25))
    for (var in varSet){
        h <- data.frame()
        j <- data.frame()
        w <- dataset
        w["x"] <- dataset[var]
        for (i in 1:25){
            q <- quantile(w$x, i / 25)
            p <- quantile(w$x, (i - 1) / 25)
            sub <- w %>% filter(x < q, x >= p)
            u <- data.frame(x = mean(sub$x))
            v <- data.frame(x = mean(sub$y))
            h <- rbind(h, u) 
            j <- rbind(j, v) 
        }
        avgPlot_y[var] <- j * 10000
        avgPlot_x[var] <- h
    }
    
    summaryTable <- function(df1){
        A1 <- as.data.frame(t(summary(df1$AR1))) %>% rename(Stat = Var2, AR1 = Freq) %>% select(Stat, AR1)
        B1 <- as.data.frame(t(summary(df1$AR2))) %>% rename(Stat = Var2, AR2 = Freq) %>% select(Stat, AR2)
        C1 <- as.data.frame(t(summary(df1$AR3))) %>% rename(Stat = Var2, AR3 = Freq) %>% select(Stat, AR3)
        D1 <- as.data.frame(t(summary(df1$EMA05))) %>% rename(Stat = Var2, EMA05 = Freq) %>% select(Stat, EMA05)
        E1 <- as.data.frame(t(summary(df1$EMA10))) %>% rename(Stat = Var2, EMA10 = Freq) %>% select(Stat, EMA10)
        F1 <- as.data.frame(t(summary(df1$CMO24))) %>% rename(Stat = Var2, CMO24 = Freq) %>% select(Stat, CMO24)
        G1 <- as.data.frame(t(summary(df1$CMO48))) %>% rename(Stat = Var2, CMO48 = Freq) %>% select(Stat, CMO48)
        H1 <- as.data.frame(t(summary(df1$MACD))) %>% rename(Stat = Var2, MACD = Freq) %>% select(Stat, MACD)
        Tab1 <- A1 %>% left_join(B1, by = "Stat") %>% left_join(C1, by = "Stat") %>% left_join(D1, by = "Stat") %>% 
            left_join(E1, by = "Stat") %>% left_join(F1, by = "Stat") %>% left_join(G1, by = "Stat") %>% 
            left_join(H1, by = "Stat")
        Table <- data.frame(Tab1)
        return(Table)
    }
    
    
    
    df1 <- dataset %>% select(Date, Time, y)
    
    output$choose_columns <- renderUI({
        checkboxGroupInput("varList", "Choose Predictor Variable Set", 
                       choiceNames = list("AR1", "AR2", "AR3", "EMA05", "EMA10", "CMO24", "CMO48","MACD"),
                       choiceValues = list("AR1", "AR2", "AR3", "EMA05", "EMA10", "CMO24", "CMO48","MACD"),
                       inline = TRUE, selected = c("AR1","EMA05","EMA10","CMO24","CMO48","MACD"))
    })
    
    df2 <- reactive({cbind(df1, dataset[, input$varList, drop = FALSE])})
    
    df3 <- reactive({ 
        a <- subset(df2(), df2()[,1] >= input$date1)
        return(a)
        })
    
    output$dataset <- renderDataTable(df3())
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("EURUSD","csv", sep=".")
        },
        content = function(file) {
            write.csv(df3(), file)
        }
    )
    
    observe({
        x <- input$varList
        
        updateRadioButtons(session, "radioButtons",
                           label = paste("Select the predictor variable:"),
                           choices = x, inline = TRUE
        )
    })
    
    
    x1 <- reactive({
        dataset[input$radioButtons]
    })
    
    x2 <- reactive({
        avgPlot_x[, input$radioButtons]
    })
    
    y <- reactive({
        avgPlot_y[, input$radioButtons]
    })
    
    output$edaPlot <- renderPlot({
        if(input$plotType == "Histogram"){
            plot_ly(x = ~x1(), type = "histogram")
        } else {
            plot(x2(), y())
        }
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function() {
            paste("EURUSD_Plot","png", sep=".")
        },
        content = function(file) {
            png(file)
        if (input$plotType == "Histogram") {
            plot_ly(dataset, x = ~dataset[unlist(input$radioButtons)], type = "histogram")
        } else {
            plot(x2(), y())
        }
        dev.off()
            
        }
    )
    
    test <- data.frame(test = 1:6)
    summTable1 <- summaryTable(dataset)
    summTable1[,-1] <- round(summTable1[,-1], 7)
    
    output$dataset2 <- renderTable({
        summTable1 %>% filter(Stat %in% input$summary)
    }, digits = 7)
    
    
    trainData <- reactive({
        head(data.frame(df3()), round(nrow(df3()) * input$trainData), 0)
    })
    
    testData <- reactive({
        head(data.frame(df3()), round(nrow(df3()) * (1 - input$trainData)), 0)
    })
    
    testY <- reactive({
        testData %>% select(y)
    })
    
    observeEvent(input$runButton, {    
        
        fit_lm <- train(y ~ .,
                        data = trainData(),
                        method = "lm",
                        trControl = trainControl(method = "cv", number = 5
                        )
        )
        
        fit_tr <- train(y ~ .,
                        data = trainData(),
                        method = "rpart",
                        trControl = trainControl(method = "cv", number = 5),
                        tuneLength = 10
                        
        )
        
        fit_rf <- train(y ~ .,
                        data = trainData(),
                        method = "rf",
                        trControl = trainControl(method = "cv", number = 5),
                        tuneGrid = data.frame(mtry = 3:6)
                        
        )
        
        output$dataset3 <- renderTable({
            fit_lm$results
        }, digits = 6)
        
        output$dataset4 <- renderTable({
            fit_tr$results
        }, digits = 6)
        
        output$dataset5 <- renderTable({
            fit_rf$results
        }, digits = 6)
        
        pred_lm <- predict(fit_lm, newdata = testData())
        output$dataset6 <- renderTable({
            postResample(pred_lm, testY())
        }, digits = 6)
        
        pred_tr <- predict(fit_tr, newdata = testData())
        output$dataset7 <- renderTable({
            postResample(pred_tr, testY())
        }, digits = 6)
        
        pred_rf <- predict(fit_rf, newdata = testData())
        output$dataset8 <- renderTable({
            postResample(pred_rf, testY())
        }, digits = 6)
        
    })
    
    preds <- reactive({
        data.frame(AR1 = input$num_AR1,
                   AR2 = input$num_AR2,
                   AR3 = input$num_AR3,
                   EMA05 = input$num_EMA05,
                   EMA10 = input$num_EMA10,
                   CMO24 = input$num_CMO24,
                   CMO48 = input$num_CMO48,
                   MACD = input$num_MACD
                       ) %>% select(input$varList)
        
        output$predLM <- renderText({predict(fit_lm, newdata = preds)})
        output$predTR <- renderText({predict(fit_tr, newdata = preds)})
        output$predRF <- renderText({predict(fit_rf, newdata = preds)})
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
