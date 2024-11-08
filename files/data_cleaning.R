dane <- read.csv("./data/online_retail.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
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

#new column with full prize 
dane$Price=dane$Quantity*dane$UnitPrice
print(max(dane$Price))
dane[dane$Price==max(dane$Price),]
dane[dane$Price==min(dane$Price),]
dane$IsReturn <- ifelse(dane$Quantity < 0, TRUE, FALSE)
liczba_zwrotow <- sum(dane$IsReturn)
print(liczba_zwrotow)
head(dane)
liczba_ujemnych_cen <- sum(dane$UnitPrice<0)
dane <- dane[dane$UnitPrice >= 0, ]
dane_na <- dane[!complete.cases(dane), ]
head(dane_na)
#widzimy ze w wiekszosci NA jest w kolumnie CustomerID co możemy zaakceptować

mask_na <- apply(dane[, !names(dane) %in% "CustomerID"], 1, function(row) any(is.na(row)))

# Wybranie wierszy, które mają NA, ale nie w kolumnie CustomerID
dane_na_elsewhere <- dane[mask_na, ]

# Wyświetlenie wierszy z brakującymi wartościami poza CustomerID
print(dane_na_elsewhere) #widzimy że NA wystepuje tylko w customer id 


head(dane)
dane$Month <- format(dane$InvoiceDate, "%Y-%m")
monthly_sales <- aggregate(Price ~ Month, data = dane, sum)
library(dplyr)
library(ggplot2)
ggplot(monthly_sales, aes(x = Month, y = Price, group = 1)) +
  geom_line() +
  labs(title = "Monthly Sales", x = "Month", y = "Total Sales")
