market_basket <- function(file) {

	library(arules)
	library(arulesViz)
	dataset <- read.csv(file)

	ds <- split(dataset$MATERIAL_NAME,dataset$X.DSRBill_Key)

	listData <- list()
	for (i in 1:length(ds)) {
		listData[[i]] <- as.character(ds[[i]][!duplicated(ds[[i]])])
	}

	Txns <- as(listData,"transactions")

	Rules<-apriori(Txns, parameter=list(supp=0.2, conf=0.4, target="rules", minlen=2))

	#itemFrequencyPlot(Txns, topN = 10)
	#inspect(Rules[1:10])
	#plot(Rules)
	
	rdf <- as(Rules,"data.frame")
	
	foo<- data.frame(do.call('rbind', strsplit(as.character(rdf$rules),'=>',fixed=TRUE)))
	
	rdf <- subset(rdf, select = c("support","confidence","lift"))
	comb <- cbind(foo,rdf)
	comb$X1 <- gsub('\\{|\\}', '', comb$X1)
	comb$X2 <- gsub('\\{|\\}', '', comb$X2)
	#head(comb)
	
	write.csv(comb,"results.csv")
}

