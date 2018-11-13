library(zoo)
library(xts)
library(TTR)
library(quantmod)

strategy = "cow.turtle"
fundmode = "full"
skt.name = "sz50"
initial.cap = 100000

skt <- getSymbols(skt.name, src="csv", col.name="Close", auto.assign=FALSE)
colnames(skt) <- "close"
print(paste(skt.name, "length:", length(index(skt))))

ma.short = 3
ma.long =120
skt.ma.short <- SMA(skt[,"close"], ma.short)
skt.ma.long <- SMA(skt[,"close"], ma.long)
print(paste("created ma data, short:", ma.short, ", long:", ma.long ))

skt.macd <- MACD(skt[,"close"], 12,26,9, maType="EMA")   #macd=DIF, signal=DEA.
print(paste("created macd data, length:", length(index(skt.macd))))

asset <- xts(matrix(nrow=length(index(skt)), ncol=2), index(skt))
colnames(asset) <- c("asset", "peak")
coredata(asset) <- rep(initial.cap, length(asset))

cross.fun <- function(vec1, vec2, id){
	if ( (id>1) && (!is.na(vec1[id-1])) && (!is.na(vec2[id-1])) 
			&& (vec1[id-1] <= vec2[id-1]) && (vec1[id] > vec2[id]) ) {
		return(TRUE)
	}
	else
		return(FALSE)
}


isBuyOpen.fun <- function(id, type="basic.macd"){
	switch (type, 
					basic.macd = isBuyOpen.basic.macd(id),
					cow.macd = isBuyOpen.cow.macd(id),
					turtle = isBuyOpen.turtle(id),
					cow.turtle = isBuyOpen.cow.turtle(id)
					)
}
isBuyOpen.basic.macd <- function(id){
	condition <- cross.fun(coredata(skt.macd$macd), coredata(skt.macd$signal), id)
	if (condition == TRUE)
		return(TRUE)
	else
		return(FALSE)
}
isBuyOpen.cow.macd <- function(id){
	condition1 <- (!is.na(coredata(skt.ma.long)[id])) & (coredata(skt.ma.short)[id] > coredata(skt.ma.long)[id])
	condition2 <- cross.fun(coredata(skt.macd$macd), coredata(skt.macd$signal), id)
	condition <- condition1 & condition2
	return(condition)
}
isBuyOpen.turtle <- function(id){
  wd <- 20
  if (id > wd) {
    condition1 <-  (coredata(skt$close)[id-1] <= max(coredata(skt$close)[(id-wd):(id-1)]))
    condition2 <- (coredata(skt$close)[id] > max(coredata(skt$close)[(id-wd):(id-1)]))
    condition <- condition1 & condition2
    return(condition)
  }
  else
    return(FALSE)
}
isBuyOpen.cow.turtle <- function(id){
  wd <- 20
  if (id > wd) {
    condition1 <-  (coredata(skt$close)[id-1] <= max(coredata(skt$close)[(id-wd):(id-1)]))
    condition2 <- (coredata(skt$close)[id] > max(coredata(skt$close)[(id-wd):(id-1)]))
    condition3 <- (!is.na(coredata(skt.ma.long)[id])) & (coredata(skt.ma.short)[id] > coredata(skt.ma.long)[id])
    condition <- condition1 & condition2 & condition3
    return(condition)
  }
  else
    return(FALSE)
}

isSellClose.fun <- function(id, type="basic.macd"){
  switch (type, 
          basic.macd = isSellClose.basic.macd(id),
          cow.macd = isSellClose.basic.macd(id),
          turtle = isSellClose.turtle(id),
          cow.turtle = isSellClose.turtle(id)
  )
}
isSellClose.basic.macd <- function(id){
	condition <- cross.fun(coredata(skt.macd$signal), coredata(skt.macd$macd), id)
	if (condition == TRUE)
		return(TRUE)
	else
		return(FALSE)
}
isSellClose.turtle <- function(id){
  wd <- 10
  if (id > wd){
    condition1 <-  (coredata(skt$close)[id-1] >= min(coredata(skt$close)[(id-wd):(id-1)]))
    condition2 <- (coredata(skt$close)[id] < min(coredata(skt$close)[(id-wd):(id-1)]))
    condition <- condition1 & condition2
    return(condition)
  }
  else
    return(FALSE)
}

hold.buy <- 0
capital <- initial.cap
buyOpen.count <- 0
sellClose.count <- 0
trade.list <- data.frame()

buyopen.fun <- function(id, fund.mode="full"){
  switch(fund.mode,
         full = buyopen.full(id),
         stepwise = buyopen.stepwise(id)
  )
}
sellclose.fun <- function(id, fund.mode="full"){
  switch(fund.mode,
         full = sellclose.full(id),
         stepwise = sellclose.stepwise(id)
  )
}
buyopen.full <- function(id){
  if (hold.buy == 0){
    buyOpen.count <<- buyOpen.count + 1	
    hold.buy <<- floor(capital/coredata(skt)[id,"close"])
    capital <<- (capital - hold.buy * coredata(skt)[id,"close"])
    
    td <- data.frame( date=index(skt)[id], op="BO", price=coredata(skt)[id,"close"], win=0,
                      holding=hold.buy, cash=capital, asset=(capital + hold.buy * coredata(skt)[id,"close"]), stringsAsFactors = FALSE)
    trade.list <<- rbind(trade.list, td)
  }
}
sellclose.full <- function(id) {
	if (hold.buy > 0) {
		sellClose.count <<- sellClose.count + 1
		capital <<- (capital + hold.buy * coredata(skt)[id,"close"])
		
		td <- data.frame( date=index(skt)[id], op="SC", price=coredata(skt)[id,"close"], win=(capital-trade.list$asset[nrow(trade.list)]),
											holding=0, cash=capital, asset=capital, stringsAsFactors = FALSE )
		trade.list <<- rbind(trade.list, td)

		hold.buy <<- 0
	}
}

hold.map <- read.csv("fundingmap.csv")
buyopen.stepwise <- function(id){
  cur_asset <- coredata(asset)[id, "asset"] #asset[id,] has been updated before calling buyopen.fun()
  if (nrow(trade.list)==0)
    cur_holding <- 0
  else
    cur_holding <- trade.list$new_holding[nrow(trade.list)]
  trade_mode <- hold.map[which(hold.map$signal=="BO" & hold.map$cur_holding==cur_holding),]
  tmp <- max( hold.buy, floor(trade_mode$next_holding * cur_asset / coredata(skt)[id,"close"]) )
  
  if(tmp > hold.buy) {
    hold.buy <<- tmp
    capital <<- (cur_asset - hold.buy * coredata(skt)[id,"close"])
    buyOpen.count <<- buyOpen.count + 1	
    
    td <- data.frame( date=index(skt)[id], op="BO", price=coredata(skt)[id,"close"], win=0,
                      holding=hold.buy, cash=capital, asset=cur_asset, 
                      new_holding=trade_mode$next_holding, stringsAsFactors = FALSE)
    trade.list <<- rbind(trade.list, td)
  }
}
sellclose.stepwise <- function(id) {
  cur_asset <- coredata(asset)[id, "asset"] #asset[id,] has been updated before calling buyopen.fun()
  if (nrow(trade.list)==0)
    cur_holding <- 0
  else
    cur_holding <- trade.list$new_holding[nrow(trade.list)]
  
  trade_mode <- hold.map[which(hold.map$signal=="SC" & hold.map$cur_holding==cur_holding),]
  tmp <- min( hold.buy, floor(trade_mode$next_holding * cur_asset / coredata(skt)[id,"close"]) )
  
  if (tmp < hold.buy) {
    hold.buy <<- tmp
    capital <<- (cur_asset - hold.buy * coredata(skt)[id,"close"])
    sellClose.count <<- sellClose.count + 1
    
    td <- data.frame( date=index(skt)[id], op="SC", price=coredata(skt)[id,"close"], 
                      win=(cur_asset-trade.list$asset[nrow(trade.list)]),
                      holding=hold.buy, cash=capital, asset=cur_asset, 
                      new_holding=trade_mode$next_holding, stringsAsFactors = FALSE )
    trade.list <<- rbind(trade.list, td)
    
  }
}

print(paste(paste(index(skt)[1], "Inital capital:", initial.cap)))

for (id in (2 : length(index(skt)))) {
  
  asset[id, "asset"] <- (capital + hold.buy * coredata(skt)[id,"close"])
  
  if ( coredata(asset)[id, "asset"] > coredata(asset)[(id-1), "peak"] ) 
  {
    asset[id, "peak"] <- asset[id, "asset"]
  }
  else
    asset[id, "peak"] <- asset[id-1, "peak"]
  
	if ( isBuyOpen.fun(id, strategy) ) {
		buyopen.fun(id, fundmode)
	}
	if ( isSellClose.fun(id, strategy) ) {
		sellclose.fun(id, fundmode)
	}
}

test_summary <- function() {
  print(paste("TEST SUMMARY of", strategy, "strategy:"))
  print(paste("buy open count:", buyOpen.count, ", sell close count:", sellClose.count))
  print(paste(index(skt)[id], "total asset:", coredata(asset$asset)[id], 
              "capital:", capital, "hold.buy:", hold.buy))
  roi <- coredata(asset$asset)[id]/initial.cap
  print(paste("ROI:", 100*(roi-1), ",stock increase:", 100*(coredata(skt$close)[id]/coredata(skt$close[1])-1),
              "%; CAGR", 100*(roi^(1/floor((as.numeric(index(skt)[id]-index(skt)[1]))/365)) - 1),
              "% from", index(skt)[1], "to", index(skt)[id]))
  print(paste("largest loss ratio:", 
              100 * max( (coredata(asset$peak) - coredata(asset$asset)) / coredata(asset$peak) ), "%",
              "; win ratio:", 100*nrow(subset(trade.list, win>0))/sellClose.count, "%" )) # or 'trade.list[which(trade.list$win>0),], or trade.list[(trade.list$win>0),]
}

test_summary()

