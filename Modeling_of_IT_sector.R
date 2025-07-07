# Завантаження бібліотек
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(corrplot)
library(psych)
library(ggcorrplot)
library(zoo)

# ======= ЧАСОВІ РЯДИ ======

# Завантаження файлу
data_q <- read_excel("it_export_q.xlsx")

# Створення об’єкта часового ряду (частота = 4 квартали на рік)
ts_data <- ts(data_q$'Компʼютерні послуги, млн.дол.CША', 
              start = c(2013, 1), 
              frequency = 4)

# Візуалізація ряду
autoplot(ts_data) + ggtitle("Експорт ІТ-послуг") + ylab("млн дол. США")

#Розкладання на тренд, сезонність і залишки
decomp <- stl(ts_data, s.window = "periodic")
autoplot(decomp)

# Тест на стаціонарність 
adf.test(ts_data)
# ряд виявися нестаціонарним, беремо перші різниці
ts_diff <- diff(ts_data)
autoplot(ts_diff)

# ARIMA
model_arima <- auto.arima(ts_diff, seasonal = TRUE)
summary(model_arima)
checkresiduals(model_arima)

# ETS 
model_ets <- ets(ts_diff)
summary(model_ets)
checkresiduals(model_ets)

# Прогнозування
predicted_diff_arima <- forecast(model_arima, h = 8)
predicted_diff_arima

xi <- tail(ts_data, 1)
predicted_level <- diffinv(forecast_arima$mean, 
                           xi = xi)
round(predicted_level, 2)

forecast_ets <- forecast(model_ets, h = 8)
forecast_ets

# Графіки прогнозів
autoplot(forecast_arima) + 
  ggtitle("ARIMA прогноз") +
  labs(x = "Рік", y = "Експорт послуг, млн дол. США") +
  scale_x_continuous(breaks = seq(2013, 2025, 1)) +
  theme_minimal(base_family = "sans")

autoplot(forecast_ets) + ggtitle("ETS прогноз")

# Порівняння результатів
accuracy(forecast_arima)
accuracy(forecast_ets)



# ==== КОРЕЛЯЦЙНИЙ АНАЛІЗ ====

# Завантаження файлу факторів
data <- read_excel("data_modeling.xlsx")

str(data)
data <- data[ , -1]
summary(data)

# Перейменування стовпців
colnames(data) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "y")

# Перевірка на нормальність
apply(data, 2, shapiro.test)

# Д
hist(data$x1, main = "Гістограма x1")
qqnorm(data$x1); qqline(data$x1)

hist(data$x2, main = "Гістограма x2")
qqnorm(data$x2); qqline(data$x2)

hist(data$x3, main = "Гістограма x3")
qqnorm(data$x3); qqline(data$x3)

hist(data$x4, main = "Гістограма x4")
qqnorm(data$x4); qqline(data$x4)

hist(data$x5, main = "Гістограма x5")
qqnorm(data$x5); qqline(data$x5)

hist(data$x6, main = "Гістограма x6")
qqnorm(data$x6); qqline(data$x6)

hist(data$x7, main = "Гістограма x7")
qqnorm(data$x7); qqline(data$x7)

hist(data$x8, main = "Гістограма x8")
qqnorm(data$x8); qqline(data$x8)

# Обчислення кореляційної матриці (коефіцієнт Пірсона)
cor_matrix <- cor(data, use = "complete.obs", method = "pearson")
round(cor_matrix, 2)

# Кореляційна матриця у вигляді діаграми
ggcorrplot(cor_matrix, 
           method = "square", 
           type = "lower",
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"),
           outline.col = "gray", 
           title = "Кореляційна матриця", 
           ggtheme = ggplot2::theme_minimal())

corr_test <- corr.test(data, use = "pairwise", method = "pearson")
corr_test$r     # коефіцієнти
corr_test$p     # p-value

# Коефіцієнт Спірмена
cor_matrix_spearman <- cor(data, method = "spearman")
corr_test_spearman <- corr.test(data, method = "spearman")
corr_test_spearman$r  # матриця коефіцієнтів
corr_test_spearman$p  # p-value для кожної пари

ggcorrplot(cor_matrix_spearman, 
           method = "square", 
           type = "lower",
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"),
           outline.col = "gray", 
           title = "Кореляційна матриця (Спірмена)", 
           ggtheme = ggplot2::theme_minimal())

round(cor_matrix_spearman, 2)



# === РЕГРЕСІЙНИЙ АНАЛІЗ ===
library(car)
library(lmtest)

# Модель stepwise
step_model <- step(lm(y ~ x2 + x3 + x4 + x5 + x6 + x8, data = data),
                   direction = "both")
summary(step_model)

vif(step_model) # Оцінка мультиколінеарності
bptest(step_model) # Перевірка гетероскедастичності
shapiro.test(residuals(step_model)) # Перевірка нормальності залишків
dwtest(step_model) # Перевірка на автокореляцію

# Візуалізація діагностики моделі
par(mfrow = c(2, 2))
plot(step_model)

#Побудова моделі №2
model_2 <- lm(y ~ x2 + x4 + x6, data = data)
summary(model_2)
vif(model_2)
bptest(model_2)
shapiro.test(residuals(model_2))
dwtest(model_2)
par(mfrow = c(2, 2))
plot(model_2)

#Побудова моделі №2.1
model_2_1 <- lm(y ~ x2 + x4 + sqrt(x6), data = data)
summary(model_2_1)
vif(model_2_1)
bptest(model_2_1)
shapiro.test(residuals(model_2_1))
dwtest(model_2_1)
par(mfrow = c(2, 2))
plot(model_2_1)

#Побудова моделі №2.2
model_2_2 <- lm(y ~ x2 + x4 + log(x6), data = data)
summary(model_2_2)
vif(model_2_2)
bptest(model_2_2)
shapiro.test(residuals(model_2_2))
dwtest(model_2_2)
par(mfrow = c(2, 2))
plot(model_2_2)

coefficients(model_2_2)
residuals(model_2_2)

library(nlme)
data$y_lag1 <- lag(data$y, 1)
model_2_3 <- lm(y ~ x2 + x4 + log(x6) + y_lag1, data = data)
summary(model_2_3)

# Припустимо, що в 2025 році очікувані значення такі:
newdata_2025 <- data.frame(
  x2 = 300000,      # Кількість IT-ФОП
  x4 = 38,          # Курс гривні до долара
  x6 = 130000       # Капітальні інвестиції (млн грн)
)

# Прогноз з логарифмічним перетворенням змінної x6
newdata_2025$logx6 <- log(newdata_2025$x6)

# Побудова прогнозу
forecast_2025 <- predict(model_2_2, 
        newdata = newdata_2025, 
        interval = "prediction", 
        level = 0.95)

years <- 2013:2024
actual <- data$y
forecast_year <- 2025


# Побудова датафрейму з усіма даними
plot_df <- data.frame(
  year = c(years, forecast_year),
  value = c(actual, forecast_2025[1]),
  type = c(rep("actual", length(actual)), "forecast"),
  lower = c(rep(NA, length(actual)), forecast_2025[2]),
  upper = c(rep(NA, length(actual)), forecast_2025[3])
)


ggplot(plot_df, aes(x = year, y = value)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(aes(color = type), size = 3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  scale_color_manual(
    values = c("actual" = "black", "forecast" = "red"),
    labels = c("Фактичні дані", "Прогноз")
  ) +
  labs(
    title = "Прогноз обсягу експорту комп’ютерних послуг на 2025 рік",
    x = "Рік",
    y = "Експорт, млн дол. США",
    color = "Тип даних"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  ) +
  geom_text(data = plot_df[nrow(plot_df), ],
            aes(label = paste0("Прогноз:\n", round(value, 0), 
                               " млн\n[", round(lower, 0), "; ", round(upper, 0), "]")),
            vjust = -1.5, hjust = 0.5, size = 4)

