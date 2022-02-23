setwd()

.libPaths()

install.packages("data.table")
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")


library(texreg)
library(readr)
library(ggplot2)
library(data.table)
library(dplyr)

transactionData <- fread(paste0("QVI_transaction_data.csv"))
purchaseData <- fread(paste0("QVI_purchase_behaviour.csv"))

### transactionData Formatting and Analysis ### 

head(transactionData)  #get handful of rows
dim(transactionData)  #find dimensions
names(transactionData)  #find column names 
str(transactionData)  #find structure of each column 
summary(transactionData)  #quick summary

# converting integer date to date formet (yyyy-mm-dd)
# note: CSV integer date starts at 1899-12-30 
transactionData$DATE <- as.Date(transactionData$DATE, origin="1899-12-30")

## remove Salsa/non chip items
transactionData <- transactionData[!grepl("salsa", tolower(transactionData$PROD_NAME))]


## finding the frequency of each product being bought
productFreq <- data.table(table(unlist(transactionData$PROD_NAME)))

productFreq$V1 <- gsub("[^a-zA-Z]+g|&|[^a-zA-Z]+G|[^a-zA-Z]+",
                        " ", productFreq$V1)

productFreq <- data.table(productFreq[order(-productFreq$N)])  # product order
# highest to lowest (descending order)

print(head(productFreq, 1))  # max frequency item
print(tail(productFreq, 1))  # min frequency item

summary(transactionData)  # notice that PROD_QTY has an outlier of 200 
# TOT_SALES also contains an outlier of $650 

outliers <- transactionData[transactionData$PROD_QTY>=10,] # using 10 as the maximum amount 
# of chips bought (find outliers)


if(nrow(outliers) == nrow(transactionData[outliers$LYLTY_CARD_NBR == 
                                          transactionData$LYLTY_CARD_NBR,])){
  sprintf("No other purchases made by Loyalty Card Number %i", outliers$LYLTY_CARD_NBR[1])  
} else{
  print(transactionData[outliers$LYLTY_CARD_NBR == 
                          transactionData$LYLTY_CARD_NBR])
}

# removing outliers
transactionData <- transactionData[transactionData$LYLTY_CARD_NBR != outliers$LYLTY_CARD_NBR]

# finding the number of transactions for each day in the financial year (365 days)
productDate <- data.table(table(unlist(transactionData$DATE)))
names(productDate) <- c("Date", "Transactions") # changing variable names
summary(productDate)  # only 364 date entry's missing 1 day 

for(i in 1:364){
  if(productDate$Date[i] == as.Date(i, origin="2018-06-30")){
    #making sure the dates are in order/not missing a date
  }else{
    missingDate <-as.Date(i, origin="2018-06-30")  # records missing date
    missingDateRow <- data.table(missingDate, NA)  # create an individual row 
    names(missingDateRow) <- names(productDate)  # making sure the names between
    # the two combining dataframes are the same
    # class(productDate$Date)
    productDate$Date <- as.Date(productDate$Date)  # ensuring date is in class "Date"
    productDate <- rbind(productDate, missingDateRow)  # combining dataframes 
    productDate<- data.table(productDate[order(productDate$Date)])  # ordering by Date
    i=1  # restart loop to check any more missing days 
  }
}

# plotting number of transaction per day over the financial year 
transactionFinancialYear <- ggplot(data=productDate, aes(x=Date, y=Transactions)) +
  geom_line() + 
  labs(x = "Date", y = "Number of Transactions", title = "Transactions
       Over Time (JUL18 - JUN19)") +
  scale_x_date(breaks = "1 month") + 
  scale_y_continuous(breaks = seq(650, 900, 100)) +
  geom_smooth(formula = y ~ x, method = "lm")
transactionFinancialYear
ggsave("transactionFinancialYear.png")

## Due to Christmas there is a spike in Dec in chip purchases as
# more people will be buying for Christmas/family events etc. 

# creating and plotting December data points 
productDateDec <- subset(productDate, "2018-12-31" >= Date & Date >= "2018-12-01")
transactionDec <- ggplot(data=productDateDec, aes(x=Date, y=Transactions)) +
  geom_line() + 
  labs(x = "Date", y = "Number of Transactions", title = "Transactions
       Over Time (DEC18)") +
  scale_x_date(breaks = "2 day") +
  scale_y_continuous(breaks = seq(650, 900, 100)) +
  geom_smooth(formula = y ~ x, method = "lm")
transactionDec
ggsave("transactionDec.png")

# ------ # 

# create a new column of packsize (removing characters and keeping numbers)
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

sizeFreq <- data.table(table(unlist(transactionData$PACK_SIZE)))
head(sizeFreq, 1)
tail(sizeFreq, 1)


packSize <- ggplot(data = sizeFreq, aes(x=sizeFreq$V1, y=sizeFreq$N)) +
  geom_col() + 
  labs(x = "Pack Size", y = "Number of Transactions", title = "Transactions
       Per Pack Size")+
  scale_x_discrete(limits = sizeFreq$V1)
packSize
ggsave("packSize.png")

# create a new column of brand names and creating a common name for each brand
transactionData[, BRAND := sub("([A-Za-z]+).*", "\\1", transactionData$PROD_NAME)]
transactionData[BRAND == "Red", BRAND := "RRD"]
transactionData[BRAND == "Rrd", BRAND := "RRD"]
transactionData[BRAND == "Dorito", BRAND := "Doritos"]
transactionData[BRAND == "Grain", BRAND := "Grnwves"]
transactionData[BRAND == "GrnWves", BRAND := "Grnwves"]
transactionData[BRAND == "Infzns", BRAND := "Infuzions"]
transactionData[BRAND == "Ncc", BRAND := "Natural"]
transactionData[BRAND == "NCC", BRAND := "Natural"]
transactionData[BRAND == "Smith", BRAND := "Smiths"]
transactionData[BRAND == "Snbts", BRAND := "Sunbites"]
transactionData[BRAND == "WW", BRAND := "Woolworths"]
transactionData[BRAND == "Burger", BRAND := "Burger"]

unique(transactionData$BRAND)

brandFreq <- data.table(table(unlist(transactionData$BRAND)))
brandFreq <- brandFreq[order(-brandFreq$N)]
head(brandFreq, 1)  # most purchased brand
tail(brandFreq, 1)  # least purchased brand

brandTrans <- ggplot(data = brandFreq, aes(x=brandFreq$V1, y=brandFreq$N)) +
  geom_col() + 
  labs(x = "Brands", y = "Number of Transactions", title = "Transactions
       Per Brand")+
  scale_x_discrete(limits = brandFreq$V1)
brandTrans
ggsave("brandTrans.png")

brandSales <- aggregate(x = transactionData$TOT_SALES, by = list(transactionData$BRAND), FUN = sum)
brandSalesGraph <- ggplot(data = brandSales, aes(x=Group.1, y=x)) +
  geom_col() + 
  labs(x = "Brands", y = "Total Sales", title = "Total Sales
       Per Brand")+
  scale_x_discrete(limits = brandSales$Group.1)
brandSalesGraph


### purchaseData Formatting and Analysis ###
nrow(purchaseData)  # number of customers in data set

# Lifestage analysis and graph
lifestageFreq <- data.table(table(unlist(purchaseData$LIFESTAGE)))
lifestageFreq <- lifestageFreq[order(-lifestageFreq$N)]
head(lifestageFreq, 1)
tail(lifestageFreq, 1)
lifestageFreq

lifestageFreqGraph <- ggplot(data = lifestageFreq, aes(x=lifestageFreq$V1, y=lifestageFreq$N)) +
  geom_col() + 
  labs(x = "Lifestage", y = "Number of Customers", title = "Number of Customers 
       per Lifestage") +
  scale_x_discrete(limit = lifestageFreq$V1)
lifestageFreqGraph
ggsave("lifestageFreqGraph.png")

# Premium Customer analysis and graph
premiumCustomerFreq <- data.table(table(unlist(purchaseData$PREMIUM_CUSTOMER)))
premiumCustomerFreq <- premiumCustomerFreq[order(-premiumCustomerFreq$N)]
head(premiumCustomerFreq)

customerClass <- ggplot(data = premiumCustomerFreq, aes(x=V1, y=N)) +
  geom_col() + 
  labs(x = "Premium Customer Class", y = "Number of Customers", title = "Number 
       of Customers per Premium Customer Class")
customerClass
ggsave("customerClass.png")

# Merging the two data sets based on Loyalty Card Number 
mergeData <- merge(transactionData, purchaseData, by='LYLTY_CARD_NBR')

# ensure all transactions have a customer linked
mergeData[which(is.null(mergeData$LIFESTAGE))] 

fwrite(mergeData, paste0("QVI_mergeData.csv"))

## Analysis ## 

# gives sum of total sales bases on lifestage and premium customer segments
totalSalesLifestage <- aggregate(x = mergeData$TOT_SALES, by = list(mergeData$LIFESTAGE), FUN = sum)
totalSalesPremiumCustomer <- aggregate(x = mergeData$TOT_SALES, by = list(mergeData$PREMIUM_CUSTOMER), FUN = sum)

# gives sum of total chip packets bought based on lifestage and prem customer segments
totalChipsLifestage <- aggregate(x = mergeData$PROD_QTY, by = list(mergeData$LIFESTAGE), FUN = sum)
totalChipsPremiumCustomer <- aggregate(x = mergeData$PROD_QTY, by = list(mergeData$PREMIUM_CUSTOMER), FUN = sum)

# ensuring all names are the same for both data tables (allow merge by common name)
names(totalSalesLifestage) <- names(lifestageFreq)
names(totalChipsLifestage) <- names(lifestageFreq)

names(totalSalesPremiumCustomer) <- names(premiumCustomerFreq)
names(totalChipsPremiumCustomer) <- names(premiumCustomerFreq)

lifestageFreq  # how many customers in each lifestage group
premiumCustomerFreq  # how many customers in each purchase behaviour group

# creating a singular data.table consisting of total sales, number of customers, 
# number of chip packets bought per segment. 
lifestageSummaryData <- merge(totalSalesLifestage, lifestageFreq, by = "V1")
lifestageSummaryData <- merge(lifestageSummaryData, totalChipsLifestage, by = "V1")

lifestageSummaryData

premiumCustomerSummaryData <- merge(totalSalesPremiumCustomer, premiumCustomerFreq, by = "V1")
premiumCustomerSummaryData <- merge(premiumCustomerSummaryData, totalChipsPremiumCustomer, by = "V1")

premiumCustomerSummaryData


# Average chip packets bought per customer (total chips/total customers per segnment)
lifestageSummaryData$aveChips <- lifestageSummaryData$N/lifestageSummaryData$N.y

averageChipsLifestageGraph <- ggplot(data = lifestageSummaryData, aes(x=V1, y=aveChips)) +
  geom_col() + 
  labs(x = "Lifestage", y = "average chip packets", title = "Average chip packets bought 
       per customer (lifestage segments)")
averageChipsLifestageGraph
ggsave("averageChipsLifestageGraph.png")

# Average sale per customer (total sales/total customers per segment)
lifestageSummaryData$aveSales <- lifestageSummaryData$N.x/lifestageSummaryData$N.y

averageSalesLifestageGraph <- ggplot(data = lifestageSummaryData, aes(x=V1, y=aveSales)) +
  geom_col() + 
  labs(x = "Lifestage", y = "average sales", title = "Average sales per customer 
       (lifestage segments)")
averageSalesLifestageGraph
ggsave("averageSalesLifestageGraph.png")

# Average chip price per customer (average Sale/average chips bought per segment)
lifestageSummaryData$aveChipPrice <- lifestageSummaryData$aveSales/
  lifestageSummaryData$aveChips

averageChipPriceLifestageGraph <- ggplot(data = lifestageSummaryData, aes(x=V1, y=aveChipPrice)) +
  geom_col() + 
  labs(x = "Lifestage", y = "average chip price", title = "Average chip price per customer 
       (lifestage segments)")
averageChipPriceLifestageGraph
ggsave("averageChipPriceLifestageGraph.png")

# Average chip packets bought per customer (total chips/total customers per segnment)
premiumCustomerSummaryData$aveChips <- premiumCustomerSummaryData$N/premiumCustomerSummaryData$N.y

averageChipsPremiumCustomerGraph <- ggplot(data = premiumCustomerSummaryData, aes(x=V1, y=aveChips)) +
  geom_col() + 
  labs(x = "Premium Customers", y = "average chip packets", title = "Average chip packets bought 
       per customer (Premium Customer Segments)")
averageChipsPremiumCustomerGraph
ggsave("averageChipsPremiumCustomerGraph.png")

# Average sale per customer (total sales/total customers per segment)
premiumCustomerSummaryData$aveSales <- premiumCustomerSummaryData$N.x/premiumCustomerSummaryData$N.y
premiumCustomerSummaryData
averageSalesPremiumCustomerGraph <- ggplot(data = premiumCustomerSummaryData, aes(x=V1, y=aveSales)) +
  geom_col() + 
  labs(x = "Premium Customer", y = "average sales", title = "Average sales per customer 
       (lifestage segments)")
averageSalesPremiumCustomerGraph

# Average chip price per customer (average Sale/average chips bought per segment)
premiumCustomerSummaryData$aveChipPrice <- premiumCustomerSummaryData$aveSales/
  premiumCustomerSummaryData$aveChips

averageChipPricePremiumCustomerGraph <- ggplot(data = premiumCustomerSummaryData, aes(x=V1, y=aveChipPrice)) +
  geom_col() + 
  labs(x = "Premium Customer", y = "average chip price", title = "Average chip price per customer 
       (lifestage segments)")
averageChipPricePremiumCustomerGraph

premiumCustomerSummaryData

# creating an individual data table for each Premium Customer Segment
budgetData <- mergeData[grepl("Budget", mergeData$PREMIUM_CUSTOMER)]
mainstreamData <- mergeData[grepl("Mainstream", mergeData$PREMIUM_CUSTOMER)]
premiumData <- mergeData[grepl("Premium", mergeData$PREMIUM_CUSTOMER)]

transactionsBudget <- budgetData[, .N, by = DATE]
transactionsBudget

transactionsMainstream <- mainstreamData[, .N, by = DATE]
transactionsPremium <- premiumData[, .N, by = DATE]
transactionsPremCust <- merge(transactionsBudget, transactionsMainstream, by = "DATE")
transactionsPremCust <- merge(transactionsPremCust, transactionsPremium, by = "DATE")
names(transactionsPremCust)[2] <- "Budget"
names(transactionsPremCust)[3] <- "Mainstream"
names(transactionsPremCust)[4] <- "Premium"

gatherTransPremCust <- melt(transactionsPremCust, id = "DATE")
ggplot(gatherTransPremCust, aes(x = DATE, y = value, colour = variable)) +
  geom_line()


brandFreqBudget <- budgetData[, .N, by = BRAND]
brandFreqMainstream <- mainstreamData[, .N, by = BRAND]
brandFreqPremium <- premiumData[, .N, by = BRAND]

brandFreqPremCust <- merge(brandFreqBudget, brandFreqMainstream, by = "BRAND")
brandFreqPremCust <- merge(brandFreqPremCust, brandFreqPremium, by = "BRAND")
names(brandFreqPremCust)[2] <- "Budget"
names(brandFreqPremCust)[3] <- "Mainstream"
names(brandFreqPremCust)[4] <- "Premium"

gatherBrandFreqPremCust <- melt(brandFreqPremCust , id.vars = "BRAND")
ggplot(gatherBrandFreqPremCust, aes(x = BRAND, y = value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  labs(title = "Brand by Affluence", y = "Number of packets")
ggsave("Brand Frequency By Affluence.png")

# ------ # 

lifesPremCustBudgetData <- data.table(table(unlist(budgetData$LIFESTAGE)))
lifesPremCustBudgetData
lifesPremCustMainstreamData <- data.table(table(unlist(mainstreamData$LIFESTAGE)))
lifesPremCustMainstreamData
lifesPremCustPremiumData <- data.table(table(unlist(premiumData$LIFESTAGE)))
lifesPremCustPremiumData

lifestagePremiumCustomerData <- merge(lifesPremCustBudgetData, lifesPremCustMainstreamData, by = "V1")
lifestagePremiumCustomerData <- merge(lifestagePremiumCustomerData, lifesPremCustPremiumData, by = "V1")

names(lifestagePremiumCustomerData)[1] <- "lifestage"
names(lifestagePremiumCustomerData)[2] <- "Budget"
names(lifestagePremiumCustomerData)[3] <- "Mainstream"
names(lifestagePremiumCustomerData)[4] <- "Premium"
lifestagePremiumCustomerData 

customers <- mergeData[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)]
numberOfCustomers <- ggplot(customers, aes(x = LIFESTAGE, y = CUSTOMERS)) + 
  geom_bar(aes(fill = PREMIUM_CUSTOMER), position = "dodge", stat = "identity") +
  labs(title = "Number of Customers Per Segment")
numberOfCustomers
ggsave("Number of Customers.png")
# plotting all Prem_Customer against each lifestage segment 
gatherData <- melt(lifestagePremiumCustomerData, id.vars = "lifestage")
gatherData
ggplot(gatherData, aes(x = lifestage, y = value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + 
  labs(title = "Total Transactions per lifestage Segment", y = "Transactions") 


# total sales per premium customer in each lifestage segment
totalSalesBudgetCustomer <- data.table(aggregate(x = budgetData$TOT_SALES, by = list(budgetData$LIFESTAGE), FUN = sum))
totalSalesMainstreamCustomer <- data.table(aggregate(x = mainstreamData$TOT_SALES, by = list(mainstreamData$LIFESTAGE), FUN = sum))
totalSalesPremiumCustomer2 <- data.table(aggregate(x = premiumData$TOT_SALES, by = list(premiumData$LIFESTAGE), FUN = sum))

lifestagePremiumCustomerTotalSalesData <- merge(totalSalesBudgetCustomer, totalSalesMainstreamCustomer, by = "Group.1")
lifestagePremiumCustomerTotalSalesData <- merge(lifestagePremiumCustomerTotalSalesData, totalSalesPremiumCustomer2, by = "Group.1")

names(lifestagePremiumCustomerTotalSalesData)[1] <- "lifestage"
names(lifestagePremiumCustomerTotalSalesData)[2] <- "Budget"
names(lifestagePremiumCustomerTotalSalesData)[3] <- "Mainstream"
names(lifestagePremiumCustomerTotalSalesData)[4] <- "Premium"
lifestagePremiumCustomerTotalSalesData

# plotting total sales for each premium customer in each lifestage segment
gatherTotalSalesData <- melt(lifestagePremiumCustomerTotalSalesData, id.vars = "lifestage")
ggplot(gatherTotalSalesData, aes(x = lifestage, y = value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  labs(title = "Total Sales per Lifestage Segment", y = "Total Sales", x = "Lifestage")  
gatherTotalSalesData
ggsave("Total Sales Per Lifestage Segment.png")

## Higher total sales for Budget Older Families ($156863.75) however there are more customers
# in Mainstream Young Singles/Couples (7917) showing that Budget Older Families either
# spend more money per chip packet/have a higher preference for more premium/expensive
# chips or buy a larger quantity of chips compared to other lifestages adn affluences 


aveSalesBudgetCustomer <- data.table(aggregate(x = budgetData$TOT_SALES, by = list(budgetData$LIFESTAGE), FUN = mean))
aveSalesMainstreamCustomer <- data.table(aggregate(x = mainstreamData$TOT_SALES, by = list(mainstreamData$LIFESTAGE), FUN = mean))
aveSalesPremiumCustomer2 <- data.table(aggregate(x = premiumData$TOT_SALES, by = list(premiumData$LIFESTAGE), FUN = mean))


lifestagePremiumCustomerAveSalesData <- merge(aveSalesBudgetCustomer, aveSalesMainstreamCustomer, by = "Group.1")
lifestagePremiumCustomerAveSalesData <- merge(lifestagePremiumCustomerAveSalesData, aveSalesPremiumCustomer2, by = "Group.1")

names(lifestagePremiumCustomerAveSalesData)[1] <- "lifestage"
names(lifestagePremiumCustomerAveSalesData)[2] <- "Budget"
names(lifestagePremiumCustomerAveSalesData)[3] <- "Mainstream"
names(lifestagePremiumCustomerAveSalesData)[4] <- "Premium"
lifestagePremiumCustomerAveSalesData

# plotting ave sales for each premium customer in each lifestage segment
gatherAveSalesData <- melt(lifestagePremiumCustomerAveSalesData, id.vars = "lifestage")
ggplot(gatherAveSalesData, aes(x = lifestage, y = value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + 
  labs(title = "Averge Sales per lifestage Segment", y = "Average Total Sales") 

totalChipsBudgetCustomer <- data.table(aggregate(x = budgetData$PROD_QTY, by = list(budgetData$LIFESTAGE), FUN = sum))
totalChipsMainstreamCustomer <- data.table(aggregate(x = mainstreamData$PROD_QTY, by = list(mainstreamData$LIFESTAGE), FUN = sum))
totalChipsPremiumCustomer2 <- data.table(aggregate(x = premiumData$PROD_QTY, by = list(premiumData$LIFESTAGE), FUN = sum))


lifestagePremiumCustomerTotalChipsData <- merge(totalChipsBudgetCustomer, totalChipsMainstreamCustomer, by = "Group.1")
lifestagePremiumCustomerTotalChipsData <- merge(lifestagePremiumCustomerTotalChipsData, totalChipsPremiumCustomer2, by = "Group.1")

names(lifestagePremiumCustomerTotalChipsData)[1] <- "lifestage"
names(lifestagePremiumCustomerTotalChipsData)[2] <- "Budget"
names(lifestagePremiumCustomerTotalChipsData)[3] <- "Mainstream"
names(lifestagePremiumCustomerTotalChipsData)[4] <- "Premium"
lifestagePremiumCustomerTotalChipsData

# plotting ave chips for each premium customer in each lifestage segment
gatherTotalChipsData <- melt(lifestagePremiumCustomerTotalChipsData, id.vars = "lifestage")
ggplot(gatherTotalChipsData, aes(x = lifestage, y = value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  labs(title = "Total Chips per lifestage Segment", y = "Total Chips")
gatherTotalChipsData

lifestagePremoumCustomer <- group_by(mergeData, LIFESTAGE, PREMIUM_CUSTOMER)
productQty <- summarise(lifestagePremoumCustomer, prod_qty = sum(PROD_QTY))
aveUnitPerCust <- customers %>% right_join(productQty, by=c("LIFESTAGE", "PREMIUM_CUSTOMER"))

aveUnitPerCust$AVEUNITPERCUSTOMER <- aveUnitPerCust$prod_qty/aveUnitPerCust$CUSTOMERS

ggplot(aveUnitPerCust, aes(x = LIFESTAGE, y = AVEUNITPERCUSTOMER)) + 
  geom_bar(aes(fill=PREMIUM_CUSTOMER), position = "dodge", stat="identity") +
  labs(title = "Average Unit by Customer Segment", y = "Units of Chips")
ggsave("Average Unit by Customer Segment.png")
## Older Families and Young Families buys more average units per customer segment
# Total Chips (QTY)/Total number of customers in segment. This is because families 
# tend to buy more chips to feed their whole family or 3+ people hence, they buy 
# more chips  

aveChipsBudgetCustomer <- data.table(aggregate(x = budgetData$PROD_QTY, by = list(budgetData$LIFESTAGE), FUN = mean))
aveChipsMainstreamCustomer <- data.table(aggregate(x = mainstreamData$PROD_QTY, by = list(mainstreamData$LIFESTAGE), FUN = mean))
aveChipsPremiumCustomer2 <- data.table(aggregate(x = premiumData$PROD_QTY, by = list(premiumData$LIFESTAGE), FUN = mean))


lifestagePremiumCustomerAveChipsData <- merge(aveChipsBudgetCustomer, aveChipsMainstreamCustomer, by = "Group.1")
lifestagePremiumCustomerAveChipsData <- merge(lifestagePremiumCustomerAveChipsData, aveChipsPremiumCustomer2, by = "Group.1")

names(lifestagePremiumCustomerAveChipsData)[1] <- "lifestage"
names(lifestagePremiumCustomerAveChipsData)[2] <- "Budget"
names(lifestagePremiumCustomerAveChipsData)[3] <- "Mainstream"
names(lifestagePremiumCustomerAveChipsData)[4] <- "Premium"
lifestagePremiumCustomerAveChipsData

# plotting ave chips for each premium customer in each lifestage segment
gatherAveChipsData <- melt(lifestagePremiumCustomerAveChipsData, id.vars = "lifestage")
ggplot(gatherAveChipsData, aes(x = lifestage, y = value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  labs(title = "Averge Chips per lifestage Segment", y = "Average Chips") 
gatherAveChipsData

lifestagePremiumCustomerAveChipsData
lifestagePremiumCustomerAveSalesData

lifestagePremiumCustomerAveChipPrice <- data.table(mapply('/', lifestagePremiumCustomerAveSalesData[,2:4], 
                                                          lifestagePremiumCustomerAveChipsData[,2:4]))

lifestagePremiumCustomerAveChipPrice$lifestage <- lifestagePremiumCustomerAveChipsData[,1]

gatherAveChipPriceData <- melt(lifestagePremiumCustomerAveChipPrice , id.vars = "lifestage")
ggplot(gatherAveChipPriceData, aes(x = lifestage, y = value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  labs(title = "Average Chip Price per Unit", x = "Lifestage", y = "Average Chip Price per Unit") 
ggsave("Average Chip Price Per Unit.png")
mergeData[, PRICE := TOT_SALES/PROD_QTY]

mainstreamSelect <- filter(mergeData, PREMIUM_CUSTOMER %in% "Mainstream", LIFESTAGE %in% "YOUNG SINGLES/COUPLES" | 
                             LIFESTAGE %in% "MIDAGE SINGLES/COUPLES")
nonMainstreamSelect <- filter(mergeData, !PREMIUM_CUSTOMER %in% "Mainstream", LIFESTAGE %in% "YOUNG SINGLES/COUPLES" |
                                LIFESTAGE %in% "MIDAGE SINGLES/COUPLES")

## Mainstream Midage and Young Singles/Couples are more willing to pay more per packet of 
# chips compared to budget and premium customers. This may be due to premium customers moving
# towards buying healthier snacks/only get chips for special occasions or for entertainment
# purposes. This is also evident by there being fewer premium customers in these segments. 

# ----- # 

mean(mainstreamSelect$PRICE)
mean(nonMainstreamSelect$PRICE)

t.test(mainstreamSelect$PRICE, nonMainstreamSelect$PRICE)
# 95% confidence  pvalue < 0.05 and hence we reject the NULL hypothesis (means=)
# thus mainstream does have a different mean then non mainstream. This means mainstream
# have a higher tendency to purchase more expensive/premium chips brands


# target segment (mainsteam)
targetSegment <- filter(mergeData, PREMIUM_CUSTOMER %in% "Mainstream", LIFESTAGE %in% "YOUNG SINGLES/COUPLES")
nonTargetSegment <- filter(mergeData, !PREMIUM_CUSTOMER %in% "Mainstream", !LIFESTAGE %in% "YOUNG SINGLES/COUPLES")

# Brand preference (affinity)
targetBrandQTY <- aggregate(targetSegment$PROD_QTY, by = list(targetSegment$BRAND), FUN = sum)
targetSum <- sum(targetSegment$PROD_QTY)
targetBrandQTY$x <- targetBrandQTY$x/targetSum

nonTargetBrandQTY <- aggregate(nonTargetSegment$PROD_QTY, by = list(nonTargetSegment$BRAND), FUN = sum)
nonTargetSum <- sum(nonTargetSegment$PROD_QTY)
nonTargetBrandQTY$x <- nonTargetBrandQTY$x/nonTargetSum

brandProportions <- merge(targetBrandQTY, nonTargetBrandQTY, by = "Group.1")
names(brandProportions)[1] <- "Brand"
names(brandProportions)[2] <- "Target Brand Affinity"
names(brandProportions)[3] <- "Non-Target Brand Affinity "

brandProportions$AffinityToBrand <- brandProportions$`Target Brand Affinity`/brandProportions$`Non-Target Brand Affinity `
brandProportions <- brandProportions[order(-brandProportions$AffinityToBrand),]
head(brandProportions)
brandAffinity <- brandProportions[, c("Brand", "AffinityToBrand")]

ggplot(brandProportions, aes(x = Brand, y = AffinityToBrand)) +
  geom_col()
## is affinity to brand > 1 then target segment has a preference to the brand. 
# we can see that the target segment prefers Tyrrells. Thus, Mainstream Young Singles/
# couples are more likely to buy Tyrrells chips compared to other brands

# Pack Size preference (affinity)
targetPackSizeQTY <- aggregate(targetSegment$PROD_QTY, by = list(targetSegment$PACK_SIZE), FUN = sum)
targetSum <- sum(targetSegment$PROD_QTY)
targetPackSizeQTY$x <- targetPackSizeQTY$x/targetSum

nonTargetPackSizeQTY <- aggregate(nonTargetSegment$PROD_QTY, by = list(nonTargetSegment$PACK_SIZE), FUN = sum)
nonTargetSum <- sum(nonTargetSegment$PROD_QTY)
nonTargetPackSizeQTY$x <- nonTargetPackSizeQTY$x/nonTargetSum

packSizeProportions <- merge(targetPackSizeQTY, nonTargetPackSizeQTY, by = "Group.1")
names(packSizeProportions)[1] <- "Pack Size"
names(packSizeProportions)[2] <- "Target Pack Size Affinity"
names(packSizeProportions)[3] <- "Non-Target Pack Size Affinity "

packSizeProportions$AffinityToPackSize <- packSizeProportions$`Target Pack Size Affinity`/packSizeProportions$`Non-Target Pack Size Affinity `
packSizeProportions <- packSizeProportions[order(-packSizeProportions$AffinityToPackSize),]
head(packSizeProportions)

packSizeAffinity <- packSizeProportions[, c("Pack Size", "AffinityToPackSize")]

## Target segment has a preference towards 270g chip pack size compared to other pack sizes
targetPackSizeBrands <- filter(mergeData, PACK_SIZE %in% "270")
unique(targetPackSizeBrands$BRAND)

## twisties are the only brand that supply 270g pack size chips

### CONCLUSION ###
## Higher total sales for Budget Older Families ($156863.75) however there are more customers
# in Mainstream Young Singles/Couples (7917) showing that Budget Older Families either
# spend more money per chip packet/have a higher preference for more premium/expensive
# chips or buy a larger quantity of chips compared to other lifestages and affluences 

## Older Families and Young Families buys more average units per customer segment
# Total Chips (QTY)/Total number of customers in segment. This is because families 
# tend to buy more chips to feed their whole family or 3+ people hence, they buy 
# more chips  

## Mainstream Midage and Young Singles/Couples are more willing to pay more per packet of 
# chips compared to budget and premium customers. This may be due to premium customers moving
# towards buying healthier snacks/only get chips for special occasions or for entertainment
# purposes. This is also evident by there being fewer premium customers in these segments.

## Target segment preffers '270g' pack size and 'Tyrrells' Brand
# Twisties are the only brand that supply 270g pack size chips 


