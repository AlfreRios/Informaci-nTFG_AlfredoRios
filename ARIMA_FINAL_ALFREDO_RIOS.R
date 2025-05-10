# -----------------------------------------------------
#  CARGA DE LIBRERÍAS
# -----------------------------------------------------
library(readxl)
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(scales)
library(Metrics)
library(ggplot2)

# -----------------------------------------------------
#  CARGA Y PREPARACIÓN DE DATOS
# -----------------------------------------------------
file_path <- "C:/Users/alfre/OneDrive/Escritorio/CUARTO AÑO/TFG/AAModelo predictivo TEMP/Datos agrupados ALFREDO RÍOS LÓPEZ/Datos_HK_Obs/DatosTempHKObs__Finales.xlsx"
data <- read_excel(file_path)
data$Date <- as.Date(data$Date)

# Agrupar y obtener promedio mensual
agg_data <- data %>%
  group_by(Date) %>%
  summarise(MeanTemp = mean(MeanTemp, na.rm = TRUE))

agg_data$YearMonth <- format(agg_data$Date, "%Y-%m")
monthly_data <- agg_data %>%
  group_by(YearMonth) %>%
  summarise(MeanTemp = mean(MeanTemp, na.rm = TRUE))

monthly_data$Fecha <- as.Date(paste0(monthly_data$YearMonth, "-01"))

# -----------------------------------------------------
#  VALIDACIÓN DEL MODELO (2010–2024)
# -----------------------------------------------------

# Dividir en entrenamiento y test
cutoff_date <- as.Date("2012-01-01")
train_data <- monthly_data %>% filter(Fecha < cutoff_date)
test_data <- monthly_data %>% filter(Fecha >= cutoff_date)

ts_train <- ts(train_data$MeanTemp,
               start = c(as.numeric(format(min(train_data$Fecha), "%Y")),
                         as.numeric(format(min(train_data$Fecha), "%m"))),
               frequency = 12)

ts_test <- ts(test_data$MeanTemp,
              start = c(as.numeric(format(min(test_data$Fecha), "%Y")),
                        as.numeric(format(min(test_data$Fecha), "%m"))),
              frequency = 12)

# -----------------------------------------------------
#  VISUALIZACION INICIAL DE LA SERIE TEMPORAL
# -----------------------------------------------------
ggplot(monthly_data, aes(x = Fecha, y = MeanTemp)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Serie Temporal de Temperatura Media Mensual",
    x = "Fecha",
    y = "Temperatura Media (°C)"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(labels = label_number(suffix = "°C")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------------------
# DESCOMPOSICION DE LA SERIE TEMPORAL
# -----------------------------------------------------

ts_data <- ts(monthly_data$MeanTemp,
              start = c(as.numeric(format(min(monthly_data$Fecha), "%Y")),
                        as.numeric(format(min(monthly_data$Fecha), "%m"))),
              frequency = 12)

# Descomposicion STL (loess) para detectar tendencia, estacionalidad y residuo
descomposicion <- stl(ts_data, s.window = "periodic")

# Graficar la descomposicion
plot(descomposicion, main = "Descomposición de la Serie Temporal")


# -----------------------------------------------------
#  ADF TEST - ESTACIONARIEDAD
# -----------------------------------------------------
cat("Prueba ADF sobre los datos de entrenamiento:\n")
adf_result <- adf.test(ts_train)
print(adf_result)

# -----------------------------------------------------
#  FAC Y FACP (ACF y PACF)
# -----------------------------------------------------
par(mfrow = c(1, 2))
acf(ts_train, main = "FAC - Temperatura Media")
pacf(ts_train, main = "FACP - Temperatura Media")
par(mfrow = c(1, 1))

# -----------------------------------------------------
#  AJUSTAR MODELO Y VALIDAR
# -----------------------------------------------------
modelo_validacion <- auto.arima(ts_train)
forecast_validacion <- forecast(modelo_validacion, h = length(ts_test))
summary(modelo_validacion)

# Extraer parámetros del modelo para usar en la predicción a futuro
order_fit <- arimaorder(modelo_validacion)

# Calcular errores
real_vs_pred <- data.frame(
  Fecha = test_data$Fecha,
  Real = ts_test,
  Predicho = forecast_validacion$mean
)
# MAE
mae_val <- mae(real_vs_pred$Real, real_vs_pred$Predicho)

# RMSE
rmse_val <- rmse(real_vs_pred$Real, real_vs_pred$Predicho)

# R² 
rss_test <- sum((real_vs_pred$Real - real_vs_pred$Predicho)^2)
tss_test <- sum((real_vs_pred$Real - mean(real_vs_pred$Real))^2)
r_squared_test <- 1 - (rss_test / tss_test)

# Residuos
residuos <- residuals(modelo_validacion)
 
# Grafico de los residuos
residuos_df <- data.frame(
  Fecha = train_data$Fecha,
  Residuos = residuos
)

ggplot(residuos_df, aes(x = Fecha, y = Residuos)) +
  geom_line(color = "darkorange", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Residuos del Modelo SARIMA ",
    x = "Fecha",
    y = "Residuos"
  ) +
  theme_minimal(base_size = 14)

#Test de Ljung-Box
ljung_box_test <- Box.test(residuos, lag = 12, type = "Ljung-Box")

cat(" VALIDACIÓN (2012–2024)\n")
cat("MAE:", round(mae_val, 3), "\n")
cat("RMSE:", round(rmse_val, 3), "\n")
cat("R²: ", r_squared_test, "\n")
cat("Valor p de la prueba de Ljung-Box: ", ljung_box_test$p.value, "\n")

# Gráfico comparacion
ggplot(real_vs_pred, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicho, color = "Predicción")) +
  labs(
    title = "SARIMA: Predicción vs Real (2012–2024)",
    x = "Fecha", y = "Temperatura Media (°C)", color = "Serie"
  ) +
  scale_color_manual(values = c("Real" = "steelblue", "Predicción" = "darkred")) +
  theme_minimal(base_size = 14)

# -----------------------------------------------------
#  PREDICCIÓN A FUTURO USANDO ORDEN CALCULADO
# -----------------------------------------------------

ts_data <- ts(monthly_data$MeanTemp,
              start = c(as.numeric(format(min(monthly_data$Fecha), "%Y")),
                        as.numeric(format(min(monthly_data$Fecha), "%m"))),
              frequency = 12)

# Volver a realizar el test de estacionariedad por si acaso
adf_result <- adf.test(ts_data)
print(adf_result)

modelo_final <- Arima(ts_data,
                      order = c(order_fit["p"], order_fit["d"], order_fit["q"]),
                      seasonal = list(order = c(order_fit["P"], order_fit["D"], order_fit["Q"]),
                                      period = 12))
#
# Errores de esta predicción
#
reales <- ts_data
ajustados <- fitted(modelo_final)
residuos <- residuals(modelo_final)

# MAE y RMSE
mae_val <- mae(reales, ajustados)
rmse_val <- rmse(reales, ajustados)

# R² 
rss <- sum((reales - ajustados)^2)
tss <- sum((reales - mean(reales))^2)
r_squared <- 1 - (rss / tss)

# Prueba de Ljung-Box 
ljung_box <- Box.test(residuos, lag = 12, type = "Ljung-Box")

# Mostrar resultados
cat("Evaluación del Modelo SARIMA sobre toda la serie:\n")
cat("R²: ", round(r_squared, 4), "\n")
cat("MAE:", round(mae_val, 4), "\n")
cat("RMSE:", round(rmse_val, 4), "\n")
cat("Ljung-Box p-valor:", round(ljung_box$p.value, 4), "\n")


# Realizar la prediccion
forecast_values <- forecast(modelo_final, h = 120)

# Crear DataFrame con predicciones
pred_df <- data.frame(
  Fecha = seq(from = max(monthly_data$Fecha) + 30, by = "month", length.out = 120),
  Prediccion = as.numeric(forecast_values$mean),
  IC_Lower_80 = as.numeric(forecast_values$lower[,1]),
  IC_Upper_80 = as.numeric(forecast_values$upper[,1]),
  IC_Lower_95 = as.numeric(forecast_values$lower[,2]),
  IC_Upper_95 = as.numeric(forecast_values$upper[,2])
)

# Graficar predicción
hist_data <- data.frame(Fecha = monthly_data$Fecha, Temperatura = monthly_data$MeanTemp)
combined_data <- rbind(
  hist_data,
  data.frame(Fecha = pred_df$Fecha, Temperatura = pred_df$Prediccion)
)

ggplot() +
  geom_line(data = hist_data, aes(x = Fecha, y = Temperatura), color = "steelblue", size = 1) +
  geom_line(data = pred_df, aes(x = Fecha, y = Prediccion), color = "orange", size = 1) +
  geom_smooth(data = combined_data, aes(x = Fecha, y = Temperatura),
              method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  geom_ribbon(data = pred_df, aes(x = Fecha, ymin = IC_Lower_95, ymax = IC_Upper_95),
              fill = "firebrick", alpha = 0.5) +
  geom_ribbon(data = pred_df, aes(x = Fecha, ymin = IC_Lower_80, ymax = IC_Upper_80),
              fill = "green", alpha = 0.5) +
  labs(
    title = "Predicción SARIMA de Temperatura Media",
    x = "Fecha", y = "Temperatura Media (°C)"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(labels = label_number(suffix = "°C")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------------------
#  GUARDAR RESULTADOS
# -----------------------------------------------------
write.xlsx(pred_df, "Prediccion_sarima_buena.xlsx")
cat("Predicciones guardadas como 'Prediccion_sarima_buena.xlsx'\n")
