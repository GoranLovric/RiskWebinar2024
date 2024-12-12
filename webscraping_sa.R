##########################################
############  Interest rates  ############
##########################################

# Web: https://data.ecb.europa.eu/data/datasets/YC/YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y

library(ecb)   # developed package for data download from the ecb website

key <- "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y"
filter <- list(lastNObservations = 100, detail = "full")

ir_1y_sven <- get_data(key, filter)

plot(ir_1y_sven$obsvalue,type='l', col="green",main = "1Y Svensson Curve", ylab="1Y IR" , xaxt='n',xlab = "Date")
axis(1, at=1:length(ir_1y_sven$obstime), labels=ir_1y_sven$obstime)
















######################################
############  Cash Flows  ############
######################################

# Web: https://www.google.com/finance/quote/APC:ETR?hl=en

library(xml2)     # allows to read html files via URL
library(rvest)    # extracts tables from html files
library(dplyr)    # chaining data operations
library(stringr)  # manipulating strings


# download html file
quote1<-"APC"
url<-paste0("https://www.google.com/finance/quote/",quote1,":ETR?hl=en")
download.file(url, destfile = "scrapedpage.html", quiet=TRUE)

# read html file -> source code
content <- read_html("scrapedpage.html")

# filter for tables in html code
test1<- content %>%  
  html_nodes("table") %>% 
  html_table()
cash_flow<-NA

print(test1)

# extract the data part
for (i in (1:length(test1))){
  if(sum(str_detect(as.data.frame(test1[[i]]), 'Cash from'))>0)     # Check the 3 tables for the term 'Cash from'
  {cash_flow<-as.data.frame(test1[[i]])}                            # if yes save it as df
}















#################################################################
############  Market risk premium - pre-defined API  ############
#################################################################

# Website: https://de.finance.yahoo.com/quote/%5EATX/chart/
# How to get API?  Investigate - Performance - Fetch/XHR
# API: https://query2.finance.yahoo.com/v8/finance/chart/%5EATX?period1=1733749200&period2=1733922000&interval=1m&includePrePost=true&events=d

library(rvest)     # to read html file
library(stringi)   # manipulating strings
library(stringr)   # manipulating strings
library(curl)      # http queries
library(lubridate) # dates
library(dplyr)     # manipulating data


# User Input
ticker_i <- '^ATX'         
start_date <- "2000-11-30" 
end_date <- "2024-01-01"   
interval <- "1mo"           


# converting dates in POSIX format for URL
start_timestamp <- as.numeric(as.POSIXct(start_date, tz = "UTC"))
end_timestamp <- as.numeric(as.POSIXct(end_date, tz = "UTC"))

# dynamic URL
url <- paste0('https://query1.finance.yahoo.com/v7/finance/chart/',ticker_i,'?',
              'period1=', start_timestamp,
              '&period2=', end_timestamp,
              '&interval=', interval)

# read data via API 
test2<-read_html(curl(url,handle = curl::new_handle("useragent" = "Python")))

# extracting content and seperating by space
test_me<-stri_split_regex(html_text2(test2), " ")

# show our extracted html
print(test_me)


# extracting relevant data and bring it in shape
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

# show final_data

# seperate date and time
ATX_data <- final_data_set %>%
  mutate(
    date = as.Date(Date),
    time = format(as.POSIXct(Date), format = "%H:%M:%S")
  )

# show ATX_data

#plot
plot(ATX_data$date, ATX_data$Close, type = "l", col = "blue",
     xlab = "Datum", ylab = "Schlusskurs", main = "ATX Schlusskurs im Zeitverlauf")
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")


