rm(list = ls())

#Setting
setwd("E:/Lab/mission4")

load("E:/Lab/mission4/base_data/CSI300Data.RData")
  
source("./code/BackTestingMatchActualReturn.R")
source("./code/TurnoverRate.R")
source("./code/ICSeries.R")
source("./code/maxdrawdown.R")
source("./code/IRYear.R")
source("./code/fCutSignal.R")
source("./code/ReviseNA.R")
source("./code/OutputTable.R")


# StockReturn
load("E:/Lab/mission4/base_data/Stock3080Return_new.RData")         # Data.Stock.Return is a list

dir <- list.files("./positive")

for(num in 1:length(dir)){
  
  # Signal
  Signal.Name <- gsub(".signal.csv", "", dir[num]) 
  Directory.Input.Signal <- c("E:/Lab/mission4/positive/")
  Data.Signal.All <- read.csv(paste0(Directory.Input.Signal, paste0(Signal.Name, ".signal.csv")), stringsAsFactors = FALSE)
  Data.Signal <- fCutSignal(Data.Signal.All)
  Data.Signal[(6275:nrow(Data.Signal)), (2:ncol(Data.Signal))] <- NA
  
  dir.create(paste0("E:/Lab/mission4/Result/", Signal.Name, "/"))
  setwd(paste0("E:/Lab/mission4/Result/", Signal.Name))
  
  # Trading days
  Tradingdays <- read.table("E:/Lab/mission4/base_data/tradingdays.csv", header = T, sep = ",", stringsAsFactors = FALSE)
  names(Tradingdays) <- c("Dayid", "Days")
  
  # Merge data
  Data.All <- list(NULL)
  for (j in 1: length(Stock.Return)) {   #need to change when you switch your stockdata
    Data.All[[j]] <- cbind(Stock.Return[[j]][, 2: 3], Signal = Data.Signal[, j + 1])
    Data.All[[j]] <- cbind(Stock.Return[[j]][, 2: 3], Signal = - Data.Signal[, j + 1]) ȡ???Ų???ʱ??
  }
  Calculation_temp <- data.frame(Date = Tradingdays[, 1], Data.All)
  Calculation <- ReviseNA(Calculation_temp)  # ????????FactorSelect??Ҫ????Ϊ??????????????ͬʱfCutSignal.RҲ??һ???Ķ?
  
  #Calculation save!!
  save(Calculation, file = paste0("./Calculation.", Signal.Name, ".RData"))
  
   Index <- which(rowSums(!is.na(Calculation[, seq(4, ncol(Calculation), 3)])) > 1000)
   # 800??500??ʼ?ո?Ϊ2007-01-15??300??Ϊ2005-04-08
   Lab_Start <- max(Index[1], match(as.integer(difftime(as.Date("2005-01-04"), as.Date("1900-01-01"))), Calculation$Date))
   Lab_End <- Index[length(Index)]
   Rebalance <- 22
   Lab_End <- match(as.integer(difftime(as.Date("2016-08-10"), as.Date("1900-01-01"))), Calculation$Date) 
   
   Result <- NewNetValue_BackTesting(Calculation, Lab_Start, Lab_End, Rebalance, CSI300Data) #Resultֻ???㵽2016-08-11??????
  # ### Result save!!
   save(Result, file = paste0("./Result.", Signal.Name, ".RData"))
   
   #IC Calculation
   IC <- ICSeries(Calculation, Lab_Start, Lab_End)
   
   ### IC Curve  save!!!
   png(paste0("./ICcurve.", Signal.Name, ".png"), width = 684, height = 507)
   plot(as.Date(Calculation$Date[Lab_Start: Lab_End], origin = "1900-01-01"), IC[Lab_Start: Lab_End], type = "l", xlab = "", ylab = "")
   title(main = paste0("Mean = ", format(mean(IC[Lab_Start: Lab_End]), scientific = T, digits = 4), "\n", "SD = ", format(sd(IC[Lab_Start: Lab_End]), scientific = T, digits = 4)), xlab = "Date", ylab = "IC")
   dev.off()
  # 
  # ### Net Value Curve  save!!!
   cols <- rainbow(4)
   DayNetValue <- Result$NetValue[Lab_Start: Lab_End, 1]
   CSINetValue <- cumprod(c(1, CSI300Data$DayProfit[Lab_Start: (Lab_End-1)]))
  # 
   Q1_NetValue <- Result$NetValue[Lab_Start: Lab_End, 2]
   Q5_NetValue <- Result$NetValue[Lab_Start: Lab_End, 3]
  # 
   MAX <- max(c(DayNetValue, CSINetValue, Q1_NetValue, Q5_NetValue))
   MIN <- min(c(DayNetValue, CSINetValue, Q1_NetValue, Q5_NetValue))
  # 
   png(paste0("./NetValue4curve.", Signal.Name, ".png"), width = 684, height = 507)
   plot(as.Date(Calculation$Date[Lab_Start:Lab_End], origin = "1900-01-01"), DayNetValue, type = "l", xlab = "", ylab = "", col = cols[1], ylim = c(MIN, MAX)) 
   lines(as.Date(Calculation$Date[Lab_Start:Lab_End], origin = "1900-01-01"), CSINetValue, type = "l", xlab = "", ylab = "", col = cols[2]) 
   lines(as.Date(Calculation$Date[Lab_Start:Lab_End], origin = "1900-01-01"), Q1_NetValue, type = "l", xlab = "", ylab = "", col = cols[3]) 
   lines(as.Date(Calculation$Date[Lab_Start:Lab_End], origin = "1900-01-01"), Q5_NetValue, type = "l", xlab = "", ylab = "", col = cols[4])
   title( xlab = "Date",ylab = "DayNetValue")
   legend('topleft', c("Q1-Q5","CSI300","Q1","Q5"), lty = 1, col = cols)
   dev.off()
  # 
   # Single Net Value Curve
   png(paste0("./NetValue1curve.", Signal.Name, ".png"), width = 684, height = 507)
   plot(as.Date(Calculation$Date[Lab_Start:Lab_End], origin = "1900-01-01"), DayNetValue, type = "l", xlab = "", ylab = "") 
   title( xlab = "Date",ylab = "Q1_Q5DayNetValue")
   dev.off()
  # 
   Output <- OutputTable(Result, IC, Tradingdays)
   write.csv(Output, file = paste0("./", Signal.Name, ".Output.csv"))
  print(paste0(Signal.Name, "finished"))
}






