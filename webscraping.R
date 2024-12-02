## Cash Flows

library(rvest)
library(xml2)
library(httr)
library(stringr)
library(stringi)
library(lubridate)
library(dplyr)

quote1<-"APC"
url<-paste0("https://www.google.com/finance/quote/",quote1,":ETR?hl=en")
download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
content <- read_html("scrapedpage.html")
test1<- content %>%  
       html_nodes("table") %>% 
       html_table()
cash_flow<-NA
for (i in (1:length(test1))){
 if(sum(str_detect(as.data.frame(test1[[i]]), 'Cash from'))>0)
 {cash_flow<-as.data.frame(test1[[i]])}
  i=i+1
  cash_flow
}


## Interest rates

library(ecb)
library(httr)    

key <- "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y"
#filter <- list(lastNObservations = 365, detail = "full")

ir_1y_sven <- get_data(key) #, filter)

plot(ir_1y_sven$obsvalue,type='l', col="green",main = "1Y Svensson Curve", ylab="1Y IR" , xaxt='n',xlab = "Date")
axis(1, at=1:length(ir_1y_sven$obstime), labels=ir_1y_sven$obstime)

## Market risk premium

require("RSQLite")

library(sqldf)
library(curl)
driverI <- dbDriver("SQLite")

#Daten holen und speichern

start_date <- "2000-11-30" 
end_date <- "2024-01-01"

ticker_i <- '^ATX'

start_timestamp <- as.numeric(as.POSIXct(start_date, tz = "UTC"))
end_timestamp <- as.numeric(as.POSIXct(end_date, tz = "UTC"))
url <- paste0('https://query1.finance.yahoo.com/v7/finance/chart/',ticker_i,'?',
              'period1=', start_timestamp,
              '&period2=', end_timestamp,
              '&interval=1mo')

test2<-read_html(curl(url,handle = curl::new_handle("useragent" = "Python")))

test_me<-stri_split_regex(html_text2(test2), " ")


mytest<-str_locate_all(pattern ='timestamp', test_me)
start_1<-str_locate_all(pattern ="\\[", test_me)[[1]][,2][str_locate_all(pattern ="\\[", test_me)[[1]][,2]>mytest[[1]][2]][1]
end_1<-str_locate_all(pattern ="\\]", test_me)[[1]][,2][str_locate_all(pattern ="\\]", test_me)[[1]][,2]>mytest[[1]][2]][1]
date_vec<-substr(test_me,start_1+1,end_1-1)
date_vec<-as.integer(matrix(unlist(stri_split_regex(date_vec, ","))))
 
mytest<-str_locate_all(pattern ='close', test_me)
start_1<-str_locate_all(pattern ="\\[", test_me)[[1]][,2][str_locate_all(pattern ="\\[", test_me)[[1]][,2]>mytest[[1]][2]][1]
end_1<-str_locate_all(pattern ="\\]", test_me)[[1]][,2][str_locate_all(pattern ="\\]", test_me)[[1]][,2]>mytest[[1]][2]][1]
close_vec<-substr(test_me,start_1+1,end_1-1)
close_vec<-as.numeric(matrix(unlist(stri_split_regex(close_vec, ","))))
final_data_set<-data.frame(Date=as_datetime(date_vec),Close=close_vec)


ATX_data <- final_data_set %>%
  mutate(
    date = as.Date(Date),
    time = format(as.POSIXct(Date), format = "%H:%M:%S")
  )
ATX_data$pct_chg1y<-NA
ATX_data$IR1y<-NA
for (i in 13:nrow(ATX_data)){
ATX_data$pct_chg1y[i]<-(ATX_data$Close[i]-ATX_data$Close[i-12])/ATX_data$Close[i-12]*100
ATX_data$IR1y[i]<- ifelse(nrow(sqldf(paste0("select obsvalue from ir_1y_sven where obstime ='",ATX_data$date[i],"'")))>0,sqldf(paste0("select obsvalue from ir_1y_sven where obstime ='",ATX_data$date[i],"'")),ATX_data$IR1y[i-1])
}
MRP_hist<-mean(as.numeric(ATX_data$pct_chg1y)-as.numeric(ATX_data$IR1y),na.rm=T)

#plot
plot(as.numeric(ATX_data$pct_chg1y)-as.numeric(ATX_data$IR1y),type='l', col="blue",  ylab = "Performance",xaxt='n',xlab = "Date", main="Historical MRP",ylim = c(-100,100))
par(new=TRUE)
plot(rep(MRP_hist,nrow(ATX_data)),type='l', col="green",ylab = "Performance", xaxt='n',xlab = "Date",ylim = c(-100,100))
axis(1, at=1:length(ATX_data$date), labels=ATX_data$date)
legend(1,100, legend=c("ATX Performance", "Average MRP"),  
       fill = c("blue","green"), cex = 0.5
)
MRP_hist







