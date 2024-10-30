dane <- read.csv("data/online_retail.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#basic info about data 
nrow(dane)
colnames(dane)
head(dane)
summary(dane)
#checking how many there is na rows
sum(is.na(dane))
#change format column with data 
dane$InvoiceDate <- as.POSIXct(dane$InvoiceDate, format="%Y-%m-%d %H:%M:%S") 
dane$InvoiceDate <- as.Date(dane$InvoiceDate)
#checking hom much time the table poses 
min_date <- min(dane$InvoiceDate, na.rm = TRUE)
print(min_date)
max_date <- max(dane$InvoiceDate, na.rm = TRUE)
print(max_date)