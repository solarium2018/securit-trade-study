library("xlsx")
library("neuralnet")
library("grid")

FILE.NAME <- "016-large.xlsx"
TRAIN_DSIZE	<- 0.7 #of inputlength
inputdata <- data.frame()
inputlength <- 0;
#########################################################################################
# Transfer excel raw data to needed relative data used as example of training data
#########################################################################################
# Read the excel sheet and remove useless rows and cols
readinput.function <- function(filename){
	print(paste("Start reading input: ", date()))
	getwd()
	inputdata <<- read.xlsx(filename, sheetIndex = 1, startRow=3, colIndex=(1:6), encoding = "UTF-8")
	#inputdata <- inputdata[-nrow(inputdata),]
	inputdata$Date <<- as.Date(inputdata$Date)
	inputlength <<- nrow(inputdata)
	TRAIN_DSIZE <<- floor(inputlength * TRAIN_DSIZE)
	print(paste(filename, " inputlength: ", inputlength))
	
	# Creat pre-day data
	inputdata.pre <- inputdata[, 2:6]
	names(inputdata.pre) <- c("pOpen", "pHigh", "pLow", "pClose", "pVol")
	inputdata.pre <- rbind(inputdata.pre[1,], inputdata.pre[1:(nrow(inputdata)-1),])
	#print(head(inputdata.pre))
	
	# Combine pre-day data with original data
	inputdata <<- cbind(inputdata, inputdata.pre)
	#print(str(inputdata))
	#print(head(inputdata))
	
	#Calculate relative value
	inputdata$vOpen <<- 100*(inputdata$Open - inputdata$pClose) / inputdata$pClose
	inputdata$vHigh <<- 100*(inputdata$High - inputdata$pClose) / inputdata$pClose
	inputdata$vLow <<- 100*(inputdata$Low - inputdata$pClose) / inputdata$pClose
	inputdata$vClose <<- 100*(inputdata$Close - inputdata$pClose) / inputdata$pClose
	inputdata$vVol <<- 100*(inputdata$VOL - inputdata$pVol) / inputdata$pVol
	#print(head(inputdata))
	print(paste("End reading input: ", date()))
	return(inputlength)
}
#####################################################################
inputlength <- readinput.function(FILE.NAME)
#--------------------------------------------------------------------
# Now the "inputdata" frame contains all needed input data to ML system
#--------------------------------------------------------------------


#########################################################################################
# Transform example data to training data format:(input1, input2, ..., inputx, output)
# Parameters: TRAIN_DSIZE	-	the number of training vector
#							N		- use N day's data as inputx.
# 						WD	- window size used to measure output value.
#							UTH1 - 3% up
#							UTH2 - 6% up
#							DTH1 - 3% down
#							DTH2 - 6% down
#							Output value	- 'UP1':[0	~	UTH1)
#														-	'UP2':[UTH1	~	UTH2)
#														-	'UP3':[UTH2	~	)
#														-	'DP1':[DTH1	~	0)
#														-	'DP2':[DTH2	~	DTH1)
#														-	'DP3':(	~	DTH2)
#########################################################################################
#--------------------------------------------------------------------
# Define constants
#--------------------------------------------------------------------
INPUT.CNT = 5
N			= 120
WD		= 1
UTH1	= 3
UTH2	= 6
DTH1	= -3
DTH2	= -6
print(paste("N: ", N, ", WD: ", WD))
norm.function <- function(a) {
	if (a < DTH2) 							{return(-3)#('DP3')
	}else if(a>=DTH2 && a<DTH1)	{return(-2)#('DP2')
	}else if(a>=DTH1 && a<0)		{return(-1)#('DP1')
	}else if(a>=0 && a<UTH1)		{return(1)#('UP1')
	}else if(a>=UTH1 && a<UTH2)	{return(2)#('UP2')
	}else												{return(3)#('UP3')
	}
}

# translate (vOpen, vHigh, vLow, vClose, vVol) into N x INPUT.CNT x (339-20) data frame
train.data <- data.frame()
date.id <- inputdata[(N+1), 1]
create_traindata.function <- function(flag="new"){
	date.id <<- inputdata[(N+1):inputlength, 1]
	filename <- paste("traindata-", substring(FILE.NAME, 1, nchar(FILE.NAME)-4), "csv", sep="")
	if (flag == "old") {
		train.data <<- read.csv(filename)
		print(paste("read train data from ", filename))
	}
	else {
		print(paste("Start creating train data: ", date()))
		for (i in ((N+1):(inputlength)) ) {
			if ((i-N)%%(floor((inputlength-N)/10))==0) print(paste("created ", 10*((i-N)%/%(floor((inputlength-N)/10))), "%"))
		# fill in N x INPUT.CNT(O/H/L/C/V) data into one row of training data.
			for (j in (N:1) ) {
				for (k in (1:INPUT.CNT) ) {
					train.data[(i-N), (N-j)*INPUT.CNT+k] <<- inputdata[i-j, 11+k]  #11->pVol, 12->vOpen, 13->vHigh
				}		
			}
		
		#	calculate output data and add to a row of training data.
			min.low <- 100 * (min(inputdata$Low[(i+1):(i+WD)]) - inputdata$Close[i]) / inputdata$Close[i]
			max.high <- 100 * (max(inputdata$High[(i+1):(i+WD)]) - inputdata$Close[i]) / inputdata$Close[i]
			train.data[(i-N), N*INPUT.CNT+1] <<- min.low
			train.data[(i-N), N*INPUT.CNT+2] <<- max.high
			train.data[(i-N), N*INPUT.CNT+3] <<- (100 * (inputdata$Close[i+WD] - inputdata$Close[i]) / inputdata$Close[i]	)
		}
		print(paste("End creating train data: ", date()))
		print(paste("train.date length : ", length(date.id), ". writting ", filename, " ... "))
		write.csv(train.data, filename, row.names=FALSE)
	}
}

#########################################################################################
create_traindata.function("old")
#create_traindata.function("new")
#########################################################################################
# Use TRAIN_DSIZE days data as training data to train the neural networks
#########################################################################################
net.low <- 0
net.high <- 0
train_net.function <- function(h=20, th=0.1){
	ch <- ""
	for (i in (1:(INPUT.CNT*N-1))) {
		ch <- paste(ch, "V", i, sep="", collaps="+")
	}
	ch <- paste(ch, "V", INPUT.CNT*N, sep="")
	#print(ch)

	print(paste("Start training f.low NET: ", date()))
	f.low <- as.formula(paste("V", INPUT.CNT*N+1, "~", ch, sep=""))
	net.low <<- neuralnet(f.low, data=train.data[1:TRAIN_DSIZE,], hidden=h, threshold=th, stepmax = 1e+09, rep = 2,
	lifesign = "full", lifesign.step=10000, learningrate = 0.1, algorithm = "rprop+", err.fct = "sse", act.fct = "logistic")
	print(paste("End training f.low NET: ", date()))

	print(paste("Start training f.high NET: ", date()))
	f.high <- as.formula(paste("V", INPUT.CNT*N+2, "~", ch, sep=""))
	net.high <<- neuralnet(f.high, data=train.data[1:TRAIN_DSIZE,], hidden=h, threshold=th, stepmax = 1e+09, rep = 2, 
	lifesign = "full", lifesign.step=10000, learningrate = 0.1, algorithm = "rprop+", err.fct = "sse", act.fct = "logistic")
	print(paste("End training f.high NET: ", date()))
	
	print(paste("Start training f.close NET: ", date()))
	f.close <- as.formula(paste("V", INPUT.CNT*N+3, "~", ch, sep=""))
	net.close <<- neuralnet(f.close, data=train.data[1:TRAIN_DSIZE,], hidden=h, threshold=th, stepmax = 1e+09, rep = 2,
	lifesign = "full", lifesign.step=10000, learningrate = 0.1, algorithm = "rprop+", err.fct = "sse", act.fct = "logistic")
	print(paste("End training f.close NET: ", date()))

	#plot(net.low)
	print(paste("Training Completed with ", TRAIN_DSIZE, "days vector!"))
}

#########################################################################################
#train_net.function(c(5,2), 0.5)
train_net.function(10, 5)
#########################################################################################
# Use the model to predict and test the result
#########################################################################################
predict.function <- function(start_row=(TRAIN_DSIZE+1), end_row=(nrow(train.data)-WD)){
	testdata <<- train.data[start_row:end_row, 1:(N*INPUT.CNT)]
	print(paste("Test data length: ", nrow(testdata)))
	
	net.low.results <<- compute(net.low, testdata)
	net.high.results <<- compute(net.high, testdata)
	net.close.results <<- compute(net.close, testdata)
}
#ls(net.low.results)
#print(net.low.results$net.result)

saveoutput.function <- function(start_row, end_row){
	output$lowact <<- train.data[start_row:end_row, (N*INPUT.CNT+1)]
	output$highact <<- train.data[start_row:end_row, (N*INPUT.CNT+2)]
	output$closeact <<- train.data[start_row:end_row, (N*INPUT.CNT+3)]
	output$lowpred <<- net.low.results$net.result
	output$highpred <<- net.high.results$net.result
	output$closepred <<- net.close.results$net.result
	#write.csv(output, "predict.csv", row.names=FALSE)
}
#########################################################################################
start.row <- (TRAIN_DSIZE+1)
end.row <- (nrow(train.data)-WD)
testdata <- data.frame()
output <- data.frame(Date = date.id[start.row:end.row], stringsAsFactors = FALSE)
net.low.results <- 0
net.high.results <- 0
net.close.results <- 0
predict.function(start.row, end.row)
saveoutput.function(start.row, end.row)
#########################################################################################

output$lowact <- train.data[start.row:end.row, (N*INPUT.CNT+1)]
output$highact <- train.data[start.row:end.row, (N*INPUT.CNT+2)]
output$closeact <- train.data[start.row:end.row, (N*INPUT.CNT+3)]
output$lowvar <- abs(output$lowpred - output$lowact)
output$highvar <- abs(output$highpred - output$highact)
output$closevar <- abs(output$closepred - output$closeact)
write.csv(output, "verify.csv", row.names=FALSE)

offsetdist.function <- function(val){
	print(paste("max of var: ", max(val)))
	print(paste("mean of var: ", mean(val)))
	print(paste("sd of var: ", sd(val)))
	print(paste("var of var: ", var(val)))

	TH <- c(0.2, 0.3, 0.5, 0.8, 1.3, 2.1, 3.4, 5.5, 8.9, 100)
	for ( i in (1:length(TH)) ) {
		print(paste("Count of < ", TH[i], ": ", length(val[val<TH[i]]), ", ", 100*length(val[val<TH[i]])/length(val), "%" ))
	}
}
print("[lowvar distribution:]")
offsetdist.function(output$lowvar)
print("[highvar distribution:]")
offsetdist.function(output$highvar)
print("[closevar distribution:]")
offsetdist.function(output$closevar)
