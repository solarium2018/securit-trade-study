library(zoo)
library(xts)
library(TTR)
library(quantmod)


initial.cap = 10000
skt.name = "SZ50"

skt.close <- getSymbols(skt.name, src="csv", col.name="Close", auto.assign=FALSE)
print(paste(skt.name, "length:", length(index(skt.close))))

skt.macd <- MACD(skt.close, 12,26,9, maType="EMA")   #macd=DIF, signal=DEA.
print(paste("created macd data, length:", length(index(skt.macd))))

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
		hold.buy <<- floor(capital/coredata(skt.close)[id])
		capital <<- (capital - hold.buy * coredata(skt.close)[id])

		td <- data.frame( date=index(skt.close)[id], op="BO", price=coredata(skt.close)[id], 
											count=hold.buy, cash=capital, asset=(capital + hold.buy * coredata(skt.close)[id]), stringsAsFactors = FALSE)
		trade.list <<- rbind(trade.list, td)
}

sellclose.fun <- function(id) {
	if (hold.buy > 0) {
		sellClose.count <<- sellClose.count + 1
		capital <<- (capital + hold.buy * coredata(skt.close)[id])
		
		td <- data.frame( date=index(skt.close)[id], op="SC", price=coredata(skt.close)[id], 
											count=hold.buy, cash=capital, asset=capital, stringsAsFactors = FALSE )
		trade.list <<- rbind(trade.list, td)	}

		hold.buy <<- 0
}

print(paste(paste(index(skt.close)[1], "Inital capital:", initial.cap)))

for (id in (2 : length(index(skt.close)))) {
	if ( isBuyOpen.fun(id, "cow.macd") ) {
		buyopen.fun(id)
	}
	if ( isSellClose.fun(id) ) {
		sellclose.fun(id)
	}
}
print(paste("buy open count:", buyOpen.count, ", sell close count:", sellClose.count))
print(paste(index(skt.close)[id], "total asset:", (capital+hold.buy*coredata(skt.close)[id]), 
			"capital:", capital, "hold.buy:", hold.buy))
print(paste("ROI:", (capital+hold.buy*coredata(skt.close)[id])/initial.cap, "from", index(skt.close)[1], "to", index(skt.close)[id]))