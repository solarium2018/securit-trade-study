library(zoo)
library(xts)
library(TTR)
library(quantmod)


initial.cap = 10000
skt.name = "SZ50"

skt <- getSymbols(skt.name, src="csv", col.name="Close", auto.assign=FALSE)
colnames(skt) <- "close"
print(paste(skt.name, "length:", length(index(skt))))

skt.macd <- MACD(skt[,"close"], 12,26,9, maType="EMA")   #macd=DIF, signal=DEA.
print(paste("created macd data, length:", length(index(skt.macd))))

asset <- xts(rep(0, length(index(skt))), index(skt))
colnames(asset) <- "asset"

cross.fun <- function(vec1, vec2, id){
	if ( (id>1) && (!is.na(vec1[id-1])) && (!is.na(vec2[id-1])) 
			&& (vec1[id-1] <= vec2[id-1]) && (vec1[id] > vec2[id]) ) {
		return(TRUE)
	}
	else  {
		return(FALSE)
	}
}


isBuyOpen.fun <- function(id, type="basic.macd"){
	switch (type, 
					basic.macd = isBuyOpen.basic.macd(id),
					cow.macd = isBuyOpen.cow.macd(id)
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
	condition1 <- (coredata(skt.macd$macd)[id] > 0)
	condition2 <- cross.fun(coredata(skt.macd$macd), coredata(skt.macd$signal), id)
	condition <- condition1 & condition2
	return(condition)
}

isSellClose.fun <- function(id){
	condition <- cross.fun(coredata(skt.macd$signal), coredata(skt.macd$macd), id)
	if (condition == TRUE)
		return(TRUE)
	else
		return(FALSE)
}

hold.buy <- 0
capital <- initial.cap
buyOpen.count <- 0
sellClose.count <- 0
trade.list <- data.frame()

buyopen.fun <- function(id){
		buyOpen.count <<- buyOpen.count + 1	
		hold.buy <<- floor(capital/coredata(skt)[id,"close"])
		capital <<- (capital - hold.buy * coredata(skt)[id,"close"])

		td <- data.frame( date=index(skt)[id], op="BO", price=coredata(skt)[id,"close"], 
											count=hold.buy, cash=capital, asset=(capital + hold.buy * coredata(skt)[id,"close"]), stringsAsFactors = FALSE)
		trade.list <<- rbind(trade.list, td)
}

sellclose.fun <- function(id) {
	if (hold.buy > 0) {
		sellClose.count <<- sellClose.count + 1
		capital <<- (capital + hold.buy * coredata(skt)[id,"close"])
		
		td <- data.frame( date=index(skt)[id], op="SC", price=coredata(skt)[id,"close"], 
											count=hold.buy, cash=capital, asset=capital, stringsAsFactors = FALSE )
		trade.list <<- rbind(trade.list, td)	}

		hold.buy <<- 0
}

print(paste(paste(index(skt)[1], "Inital capital:", initial.cap)))

for (id in (2 : length(index(skt)))) {
  asset[id, "asset"] <- (capital + hold.buy * coredata(skt)[id,"close"])
  
	if ( isBuyOpen.fun(id, "cow.macd") ) {
		buyopen.fun(id)
	}
	if ( isSellClose.fun(id) ) {
		sellclose.fun(id)
	}
}
print(paste("buy open count:", buyOpen.count, ", sell close count:", sellClose.count))
print(paste(index(skt)[id], "total asset:", (capital+hold.buy*coredata(skt)[id,"close"]), 
			"capital:", capital, "hold.buy:", hold.buy))
print(paste("ROI:", (capital+hold.buy*coredata(skt)[id,"close"])/initial.cap, 
            "from", index(skt)[1], "to", index(skt)[id]))

