
library(tidyverse)
library(RcppRoll)
library(caret)
library(kableExtra)

data2015 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_2015.csv")
data2016 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_2016.csv")
data2017 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_2017.csv")
data2018 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_2018.csv")
data2019 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_2019.csv")
data2020 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_2020.csv")
data2021_01 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_202101.csv")
data2021_02 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_202102.csv")
data2021_03 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_202103.csv")
data2021_04 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_202104.csv")
data2021_05 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_202105.csv")
data2021_06 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_202106.csv")
data2021_07 <- read_csv("C:/Users/rm915/Desktop/ST 558/Project3/DAT_MT_EURUSD_M1_202107.csv")


data <- data.frame(rbind(data2015, data2016, data2017, data2018, data2019, data2020,
              data2021_01, data2021_02, data2021_03, data2021_04, data2021_05, data2021_06, data2021_07))

#Reduce the dataset to hourly closing values
data$min  <- format(strptime(data$Time,"%H:%M:%S"),'%M') 
data$hour <- format(strptime(data$Time,"%H:%M:%S"),'%H') 
data <- data %>% filter(min == "00", hour %in% c('00', '03', '06', '09', '12', '15', '18', '21')) %>% select(Date, Time, Close)

#Reformatting the date variable to be compatible with R
data$Date <- as.Date(paste0(str_sub(data$Date,1, 4),"/",str_sub(data$Date,6,7),"/",str_sub(data$Date,-2)))

#Creating dependent variables - 1, 2 & 3 hour change in the exchange rate
data <- data %>% arrange(Date, Time)
data <- data %>% mutate(y = round(lead(Close, 1) - Close, 6))


#Creating autoregressive variables
data <- data %>% mutate(AR1 = round(Close - lag(Close, 1),6))
data <- data %>% mutate(AR2 = round(Close - lag(Close, 2),6))
data <- data %>% mutate(AR3 = round(Close - lag(Close, 3),6))

#Creating Technical Analysis variables

#EMA Function - exponential moving average (this is a computationally inexpensive estimate)
EMA <- function(period, var){
  alpha <- 2 / (period + 1)
  period_length <- as.integer(round(7.5 * period, 0))
  x <- var
  tempdata <- data.frame(x) %>% mutate(z = x * alpha)
  for (i in 1:period_length){
    tempdata$z <- tempdata$z + alpha * lag(tempdata$x, i) * ((1 - alpha)^i)
  }
  return(tempdata$z)
}

data <- data %>% mutate(EMA05 = round(EMA(5, data$Close) - Close, 8), EMA10 = round(EMA(10, data$Close) - Close, 8))

#MACD
data <- data %>% mutate(MACD_Line = EMA(12, data$Close) - EMA(26, data$Close)) 
data <- data %>% mutate(MACD = round(MACD_Line - EMA(9, data$MACD_Line), 8)) %>% select(-MACD_Line)

#CMO function
CMO <- function(period, var){
  x <- var
  x <- data.frame(x)
  x <- x %>% mutate(diff = x - lag(x)) %>% mutate(up = ifelse(diff > 0, 1, 0), down = ifelse(diff < 0, 1, 0))
  x <- data.frame(x[period:nrow(x),], SumUp = roll_sum(x$up, period, align = "right"), SumDn = roll_sum(x$down, period, align = "right"))
  x <- x %>% mutate(cmo = (SumUp - SumDn) / period)
  return(x$cmo)
}

data <- data.frame(data[24:nrow(data),], CMO24 = round(CMO(24, data$Close),8))
data <- data.frame(data[48:nrow(data),], CMO48 = round(CMO(48, data$Close),8))
data <- na.omit(data)

write.csv(data,"C:\\Users\\rm915\\Desktop\\ST 558\\Project3\\EURUSD_data.csv", row.names = FALSE)

train <- data %>% filter(Date < '2019-01-01')
train1 <- train %>% select(-Date, -Time, -Close, -y2, -y3)
train2 <- train %>% select(-Date, -Time, -Close, -y1, -y3)
train3 <- train %>% select(-Date, -Time, -Close, -y1, -y2)
test <- data %>% filter(Date >= '2019-01-01')
test1 <- test %>% select(-Date, -Time, -Close, -y2, -y3)
test2 <- test %>% select(-Date, -Time, -Close, -y1, -y3)
test3 <- test %>% select(-Date, -Time, -Close, -y1, -y2)

#Set Seed
set.seed(0123)
#Random Forest Model
fit_rf <- train(y ~ .,
                data =  train1,
                method = "rf",
                trControl = trainControl(method = "cv", number = 4),
                tuneGrid = data.frame(mtry = 3:6))

#Print Statistics
kable(fit_rf$results, caption = "Fit Statistics for 1 Hour Model", digits = 4) %>% kable_styling()
fit_rf$results
# Print Summary 
print(summary(fit_rf))

pred_rf <- predict(fit_rf, newdata = test1)
kable(postResample(pred_rf, test1$y1), caption = "Fit Statistics for One Step Ahead Model", digits = 3) %>% kable_styling()

test$pred1 <- pred_rf1


write.csv(test,"C:\\Users\\rm915\\Desktop\\ST 558\\Project3\\test.csv", row.names = FALSE)

dataCols <- data %>% select(Date, Time, y)

s <- data.frame()
for (i in 1:25){
  q <- quantile(data$AR1, i/25)
  p <- quantile(data$AR1, (i-1)/25)
  sub <- data %>% filter(AR1 > p, AR1 < q)
  u <- data.frame(AR1 = c((p + q) / 2))
  u$y <- mean(sub$y)
  s <- rbind(s, u)
}



varSet <- c("AR1", "AR2", "AR3", "EMA05", "EMA10", "CMO24", "CMO48","MACD")
avgPlot_y = data.frame(index = c(1:25))
avgPlot_x = data.frame(index = c(1:25))
for (var in varSet){
  h <- data.frame()
  j <- data.frame()
  w <- data
  w["x"] <- data[var]
  for (i in 1:25){
    q <- quantile(w["x"], i / 25)
    p <- quantile(w["x"], (i - 1) / 25)
    sub <- subset(w, x > p, x < q)
    u <- data.frame(x = mean(sub["x"]))
    v <- data.frame(x = mean(sub["y"]))
    h <- rbind(h, u) 
    j <- rbind(j, v) 
  }
  avgPlot_y[var] <- j
  avgPlot_x[var] <- h
}




varSet <- list("AR1", "AR2", "AR3", "EMA05", "EMA10", "CMO24", "CMO48","MACD")
avgPlot_y = data.frame(index = c(1:25))
avgPlot_x = data.frame(index = c(1:25))
for (var in varSet){
  h <- data.frame()
  j <- data.frame()
  w <- data
  w["x"] <- data[var]
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
    left_join(H1, by = "Stat") %>% rename(" " = "Stat")
  Table <- data.frame(Tab1)
  return(Table)
}

test <- data.frame(test = 1:6)
summTable1 <- summaryTable(data)
summTable1[,-1] <- round(summTable1[,-1], 7)
summTable2 <- cbind(test, summTable1)

