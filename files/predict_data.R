colnames(dane)
library(dplyr)
library(forecast)
library(ggplot2)
library(caret)
str(dane)

df <- dane

df$Year <- as.factor(df$Year)
df$Month <- as.factor(df$Month)
df$DayOfWeek <- as.factor(df$DayOfWeek)
df$IsReturn <- as.factor(df$IsReturn)
df$Country <- as.factor(df$Country)
str(df)

df_klient <- df %>%
  group_by(CustomerID) %>%
  summarise(
    Number_of_orders = n(),  # Liczba zamówień klienta
    Total_customer_sales = sum(Price, na.rm = TRUE),  # Suma wszystkich zamówień tego klienta
    Average_order_value = mean(Price, na.rm = TRUE)  # Średnia wartość zamówienia dla klienta
  )
df <- left_join(df, df_klient, by = "CustomerID")
head(df)
# 6. Usunięcie kolumn, które nie są potrzebne do regresji
# Usuniemy: InvoiceNo, StockCode, Description, InvoiceDate (bo mamy już Year, Month, DayOfWeek)
colnames(df)
unique(df$Country )
country_orders <- df %>%
  group_by(Country) %>%
  summarise(Number_of_orders = n())

# Wyświetlenie wyników
print(country_orders,n=37)

countries_to_remove <- country_orders %>%
  filter(Number_of_orders < 70) %>%
  pull(Country)  # Pobranie listy nazw krajów do usunięcia

# 5. Usunięcie wierszy, gdzie Country jest jednym z krajów do usunięcia
df <- df %>%
  filter(!Country %in% countries_to_remove)

country_orders_after <- df %>%
  group_by(Country) %>%
  summarise(Number_of_orders = n()) %>%
  arrange(desc(Number_of_orders))

# 7. Wyświetlenie wyników po usunięciu
print(country_orders_after, n = nrow(country_orders_after)) 
unique(df$Country)
colnames(df)
df <- df %>%
  select(-c(InvoiceNo, StockCode, Description, InvoiceDate))


#7. Przygotowanie danych do regresji
# Zmienna zależna: Price (całkowita wartość zamówienia)
# Zmienne niezależne (predyktory)
X <- df %>%
  select(Quantity, UnitPrice, DayOfWeek, Month, Year, IsReturn, Number_of_orders, 
         Total_customer_sales, Average_order_value, Country)
y <- df$Price

# 8. Podział danych na zbiór treningowy i testowy (80/20)
set.seed(42)
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# 9. Budowanie modelu regresji liniowej
model <- lm(Price ~ Quantity + UnitPrice + DayOfWeek + Month +  IsReturn  + 
               Total_customer_sales+Average_order_value+Country , 
            data = train_data)

# 10. Wyświetlenie podsumowania modelu
summary(model)




# 11. Prognozowanie na zbiorze testowym
predictions <- predict(model, test_data)

# 12. Ocena jakości modelu
# Średni błąd kwadratowy (MSE)
mse <- mean((predictions - test_data$Price)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Pierwiastek z MSE (RMSE)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Średni błąd absolutny (MAE)
mae <- mean(abs(predictions - test_data$Price))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Średni procentowy błąd absolutny (MAPE)
mape <- mean(abs((predictions - test_data$Price) / test_data$Price)) * 100
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")

# Współczynnik determinacji R²
r_squared <- summary(model)$r.squared
cat("R-squared (R²):", r_squared, "\n")

#DayOfWeek – Największe zamówienia składane są w niedziele, a najniższe w środy.
#Month – Zamówienia w kwietniu, lipcu i sierpniu są niższe.
#maj czerwiec i grudzien najwoecej zamowien było 
#Jakość modelu – Wysokie R² = 0.9244 wskazuje, że model bardzo dobrze tłumaczy zmienność zmiennej Price.
#Zalecenia:
 # Zwiększ sprzedaż w środy i miesiące letnie (np. promocje). bo maja najmiejsze  value cyzli sa najbardziej istotne 
#Skup się na krajach, które generują większe zamówienia (Austria, Belgia, Niemcy).
#Zidentyfikuj klientów, którzy często składają małe zamówienia, i zachęć ich do większych zakupów (np. oferty promocyjne).


plot(test_data$Price, predictions, main="Rzeczywista vs Prognozowana cena",
     xlab="Rzeczywista cena", ylab="Prognozowana cena", pch=19)
abline(0, 1, col="red")

num_vars <- df %>%
  select(Quantity, UnitPrice, Number_of_orders, Total_customer_sales, Average_order_value, Price)

cor_matrix <- cor(num_vars, use = "complete.obs")
print(cor_matrix)



# 4. Model bazowy (bez zmiennych niezależnych)
model_base <- lm(Price ~ 1, data = train_data)

# 5. Zakres zmiennych do uwzględnienia
scope <- as.formula(Price ~ Quantity + UnitPrice + DayOfWeek + Month + 
                      IsReturn + Total_customer_sales + Average_order_value + Country)


# 6. Forward selection
model_forward <- step(model_base, scope = scope, direction = "forward")

# 7. Wyświetlenie podsumowania modelu
summary(model_forward)

model2<-lm(Price~ Quantity + UnitPrice + DayOfWeek + Month + 
             IsReturn + Total_customer_sales + Average_order_value, data = train_data)
summary(model2)




# 11. Prognozowanie na zbiorze testowym
predictions2 <- predict(model2, test_data)

# 12. Ocena jakości modelu
# Średni błąd kwadratowy (MSE)
mse2 <- mean((predictions2 - test_data$Price)^2)
cat("Mean Squared Error (MSE):", mse2, "\n")

# Pierwiastek z MSE (RMSE)
rmse2 <- sqrt(mse2)
cat("Root Mean Squared Error (RMSE):", rmse2, "\n")

# Średni błąd absolutny (MAE)
mae2 <- mean(abs(predictions2 - test_data$Price))
cat("Mean Absolute Error (MAE):", mae2, "\n")


# Współczynnik determinacji R²
r_squared2 <- summary(model2)$r.squared
cat("R-squared (R²):", r_squared2, "\n")