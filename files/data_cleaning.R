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



#widzimy ze w wiekszosci NA jest w kolumnie CustomerID co możemy zaakceptować

mask_na <- apply(dane[, !names(dane) %in% "CustomerID"], 1, function(row) any(is.na(row)))

# Wybranie wierszy, które mają NA, ale nie w kolumnie CustomerID
dane_na_elsewhere <- dane[mask_na, ]

# Wyświetlenie wierszy z brakującymi wartościami poza CustomerID
print(dane_na_elsewhere) #widzimy że NA wystepuje tylko w customer id 

#dropping the NA data 
dane <- na.omit(dane)

#adding new column to visualize data easier
dane$Year <- format(as.Date(dane$InvoiceDate), "%Y")
dane$Month <- format(as.Date(dane$InvoiceDate), "%m")
dane$DayOfWeek <- weekdays(as.Date(dane$InvoiceDate))

head(dane)

library(dplyr)
library(ggplot2)

#first plot to show how much people buy a year, month
monthly_sales <- dane %>%
  group_by(Year, Month) %>%
  summarise(TotalSales = sum(Price, na.rm = TRUE)) %>%
  ungroup()

# Stworzenie dodatkowej kolumny "YearMonth" do lepszej wizualizacji
monthly_sales <- monthly_sales %>%
  mutate(YearMonth = paste(Year, Month, sep = "-"))

ggplot(monthly_sales, aes(x = YearMonth, y = TotalSales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Monthly Sales Analysis", x = "Year-Month", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#podział na dni tygodnia
day_of_week_sales <- dane %>%
  group_by(DayOfWeek) %>%
  summarise(TotalSales = sum(Price, na.rm = TRUE)) %>%
  ungroup()

day_of_week_sales$DayOfWeek <- factor(day_of_week_sales$DayOfWeek, 
                                      levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))

# Wykres słupkowy pokazujący sprzedaż w podziale na dni tygodnia
ggplot(day_of_week_sales, aes(x = DayOfWeek, y = TotalSales)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Sales by Day of the Week", x = "Day of the Week", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3. Najlepiej Sprzedające się Produkty
# Agregowanie sprzedaży w zależności od produktu
best_selling_products <- dane %>%
  group_by(Description) %>%
  summarise(TotalSales = sum(Price, na.rm = TRUE)) %>%
  arrange(desc(TotalSales)) %>%
  slice_head(n = 20) %>%  # Wybór 10 najlepiej sprzedających się produktów
  ungroup()

# Wykres słupkowy pokazujący najlepiej sprzedające się produkty
ggplot(best_selling_products, aes(x = reorder(Description, -TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 Best Selling Products", x = "Product Description", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 4. Sprzedaż w Podziale na Kraje
# Agregowanie sprzedaży w zależności od kraju
country_sales <- dane %>%
  group_by(Country) %>%
  summarise(TotalSales = sum(Price, na.rm = TRUE)) %>%
  ungroup()

# Wykres słupkowy pokazujący sprzedaż w podziale na kraje
ggplot(country_sales, aes(x = reorder(Country, -TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Sales by Country", x = "Country", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

best_selling_products_uk <- dane %>%
  filter(Country == "United Kingdom") %>%
  group_by(Description) %>%
  summarise(TotalSales = sum(Price, na.rm = TRUE)) %>%
  arrange(desc(TotalSales)) %>%
  slice_head(n = 5) %>%  # Wybór 5 najlepiej sprzedających się produktów w UK
  ungroup()

# Wykres słupkowy pokazujący najlepiej sprzedające się produkty w UK
ggplot(best_selling_products_uk, aes(x = reorder(Description, -TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 5 Best Selling Products in the UK", x = "Product Description", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

best_selling_products_netherland <- dane %>%
  filter(Country == "Netherlands") %>%
  group_by(Description) %>%
  summarise(TotalSales = sum(Price, na.rm = TRUE)) %>%
  arrange(desc(TotalSales)) %>%
  slice_head(n = 5) %>%  # Wybór 5 najlepiej sprzedających się produktów w UK
  ungroup()

# Wykres słupkowy pokazujący najlepiej sprzedające się produkty w UK
ggplot(best_selling_products_netherland, aes(x = reorder(Description, -TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 5 Best Selling Products in the netherlands", x = "Product Description", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

best_selling_products_eire <- dane %>%
  filter(Country == "EIRE") %>%
  group_by(Description) %>%
  summarise(TotalSales = sum(Price, na.rm = TRUE)) %>%
  arrange(desc(TotalSales)) %>%
  slice_head(n = 5) %>%  # Wybór 5 najlepiej sprzedających się produktów w UK
  ungroup()

# Wykres słupkowy pokazujący najlepiej sprzedające się produkty w UK
ggplot(best_selling_products_eire, aes(x = reorder(Description, -TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 5 Best Selling Products in the eire", x = "Product Description", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

unique_products_count <- dane %>%
  summarise(NumUniqueProducts = n_distinct(Description))

# Wyświetlenie wyniku - tyle różnych porduktó
print(unique_products_count)
