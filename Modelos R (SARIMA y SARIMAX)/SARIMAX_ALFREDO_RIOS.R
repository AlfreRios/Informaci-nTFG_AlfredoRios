# -----------------------------------------------------
# CARGA DE LIBRERÍAS
# -----------------------------------------------------
library(readxl)
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(scales)
library(Metrics)

# -----------------------------------------------------
# CARGA Y PREPARACIÓN DE DATOS
# -----------------------------------------------------
file_path <- "C:/Users/alfre/OneDrive/Escritorio/CUARTO AÑO/TFG/AAModelo predictivo TEMP/Datos agrupados ALFREDO RÍOS LÓPEZ/Datos_HK_Obs/DatosTempHKObs__Finales.xlsx"
data <- read_excel(file_path)
data$Date <- as.Date(data$Date)

# Agrupar por fecha
agg_data <- data %>%
  group_by(Date) %>%
  summarise(
    MeanTemp = mean(MeanTemp, na.rm = TRUE),
    Nubes = mean(Nubes, na.rm = TRUE),
    Humedad = mean(Humedad, na.rm = TRUE),
    Precipitacion = mean(Precipitacion, na.rm = TRUE),
    Presion = mean(Presion, na.rm = TRUE)
  )

# Convertir a datos mensuales
agg_data$YearMonth <- format(agg_data$Date, "%Y-%m")
monthly_data <- agg_data %>%
  group_by(YearMonth) %>%
  summarise(
    MeanTemp = mean(MeanTemp, na.rm = TRUE),
    Nubes = mean(Nubes, na.rm = TRUE),
    Humedad = mean(Humedad, na.rm = TRUE),
    Precipitacion = mean(Precipitacion, na.rm = TRUE),
    Presion = mean(Presion, na.rm = TRUE)
  )

monthly_data$Fecha <- as.Date(paste0(monthly_data$YearMonth, "-01"))

# -----------------------------------------------------
# DIVISIÓN EN TRAIN Y TEST
# -----------------------------------------------------
cutoff_date <- as.Date("2012-01-01")
train_data <- monthly_data %>% filter(Fecha < cutoff_date)
test_data <- monthly_data %>% filter(Fecha >= cutoff_date)

# Series temporales
ts_train <- ts(train_data$MeanTemp,
               start = c(as.numeric(format(min(train_data$Fecha), "%Y")),
                         as.numeric(format(min(train_data$Fecha), "%m"))),
               frequency = 12)

ts_test <- ts(test_data$MeanTemp,
              start = c(as.numeric(format(min(test_data$Fecha), "%Y")),
                        as.numeric(format(min(test_data$Fecha), "%m"))),
              frequency = 12)

xreg_train_presion <- as.matrix(train_data[, "Presion", drop = FALSE])
xreg_test_presion <- as.matrix(test_data[, "Presion", drop = FALSE])

# Escalar la variable Presión
xreg_train_presion_scaled <- scale(xreg_train_presion)
xreg_test_presion_scaled <- scale(xreg_test_presion, 
                                  center = attr(xreg_train_presion_scaled, "scaled:center"),
                                  scale = attr(xreg_train_presion_scaled, "scaled:scale"))


# -----------------------------------------------------
#  ADF TEST - ESTACIONARIEDAD
# -----------------------------------------------------
cat("Prueba ADF sobre los datos de entrenamiento:\n")
adf_result <- adf.test(ts_train)
print(adf_result)

# Crear series temporales para la variable exógena y asi comprobar su estacionariedad

year_min <- as.numeric(format(min(train_data$Fecha), "%Y"))
month_min <- as.numeric(format(min(train_data$Fecha), "%m"))

ts_presion <- ts(train_data$Presion, start = c(year_min, month_min), frequency = 12)
adf.test(ts_presion)

plot(ts_presion, main = "Serie Temporal: Presión", ylab = "hPa")


# -----------------------------------------------------
# AJUSTAR MODELO SARIMAX CON auto.arima SOLO CON PRESION
# -----------------------------------------------------
modelo_sarimax_presion <- auto.arima(ts_train,
                                     xreg = xreg_train_presion_scaled,
                                     seasonal = TRUE,
                                     stepwise = FALSE,
                                     approximation = FALSE)
summary(modelo_sarimax_presion)

# Predicción sobre test usando solo presion
forecast_sarimax_presion <- forecast(modelo_sarimax_presion, 
                                     xreg = xreg_test_presion_scaled, 
                                     h = length(ts_test))

# Crear dataframe con resultados
real_vs_pred_presion <- data.frame(
  Fecha = test_data$Fecha,
  Real = ts_test,
  Predicho = forecast_sarimax_presion$mean
)

# Calcular métricas para evaluar el modelo
mae_val_presion <- mae(real_vs_pred_presion$Real, real_vs_pred_presion$Predicho)
rmse_val_presion <- rmse(real_vs_pred_presion$Real, real_vs_pred_presion$Predicho)
rss_presion <- sum((real_vs_pred_presion$Real - real_vs_pred_presion$Predicho)^2)
tss_presion <- sum((real_vs_pred_presion$Real - mean(real_vs_pred_presion$Real))^2)
r_squared_test_presion <- 1 - rss_presion / tss_presion
mape_val_presion <- mean(abs((real_vs_pred_presion$Real - real_vs_pred_presion$Predicho) / real_vs_pred_presion$Real)) * 100


# Residuos y prueba de Ljung-Box y Shaphiro Test
residuos_presion <- residuals(modelo_sarimax_presion)
ljung_box_test_presion <- Box.test(residuos_presion, lag = 12, type = "Ljung-Box")


# Mostrar métricas
cat("MAE:", round(mae_val_presion, 3), "\n")
cat("RMSE:", round(rmse_val_presion, 3), "\n")
cat("R²:", round(r_squared_test_presion, 3), "\n")
cat("Valor p de Ljung-Box:", round(ljung_box_test_presion$p.value, 4), "\n")
cat("MAPE:", round(mape_val_presion, 3), "\n")

# -----------------------------------------------------
# GRÁFICO COMPARATIVO
# -----------------------------------------------------
ggplot(real_vs_pred_presion, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicho, color = "Predicción")) +
  labs(
    title = "SARIMAX: Predicción vs Real (2012–2024)",
    x = "Fecha", y = "Temperatura Media (°C)", color = "Serie"
  ) +
  scale_color_manual(values = c("Real" = "steelblue", "Predicción" = "darkred")) +
  theme_minimal(base_size = 14)

# -----------------------------------------------------
# ENTRENAMIENTO FINAL SOBRE TODA LA SERIE + PREDICCIÓN A FUTURO
# -----------------------------------------------------

# Serie completa de temperatura
ts_all <- ts(monthly_data$MeanTemp,
             start = c(as.numeric(format(min(monthly_data$Fecha), "%Y")),
                       as.numeric(format(min(monthly_data$Fecha), "%m"))),
             frequency = 12)

# Solo presión como variable exógena
xreg_all <- as.matrix(monthly_data[, "Presion", drop = FALSE])

# Escalar presión
xreg_all_scaled <- scale(xreg_all)

# Ajustar modelo SARIMAX final con el orden óptimo que hallaste (del modelo anterior)
modelo_final <- Arima(ts_all,
                      order = arimaorder(modelo_sarimax_presion)[1:3],
                      seasonal = list(order = arimaorder(modelo_sarimax_presion)[4:6], period = 12),
                      xreg = xreg_all_scaled)

# -----------------------------------------------------
# ERRORES SOBRE EL MODELO FINAL ENTRENADO (SARIMAX con Presión)
# -----------------------------------------------------

# Serie completa de temperatura y valores ajustados
ts_data <- ts_all  # Temperatura observada
ajustados <- fitted(modelo_final)  # Valores ajustados por el modelo
residuos <- residuals(modelo_final)  # Residuos del modelo

# Cálculo de métricas de error
mae_val <- mae(ts_data, ajustados)
rmse_val <- rmse(ts_data, ajustados)
mape_val <- mean(abs((ts_data - ajustados) / ts_data)) * 100

# Coeficiente de determinación R²
rss <- sum((ts_data - ajustados)^2)
tss <- sum((ts_data - mean(ts_data))^2)
r_squared <- 1 - (rss / tss)

# Prueba de Ljung-Box para independencia de residuos
ljung_box <- Box.test(residuos, lag = 12, type = "Ljung-Box")

# Resultados
cat("Evaluación del Modelo SARIMAX final (Presión como exógena):\n")
cat("R²: ", round(r_squared, 4), "\n")
cat("MAE:", round(mae_val, 4), "\n")
cat("RMSE:", round(rmse_val, 4), "\n")
cat("Ljung-Box p-valor:", round(ljung_box$p.value, 4), "\n")
cat("MAPE:", round(mape_val, 4), "\n")

# Número de períodos futuros (10 años * 12 meses)
h <- 120

# Crear variable exógena futura: media histórica escalada de presión
mean_presion_scaled <- colMeans(xreg_all_scaled, na.rm = TRUE)
future_xreg <- matrix(rep(mean_presion_scaled, h),
                      nrow = h, ncol = 1, byrow = TRUE)
colnames(future_xreg) <- "Presion"

# Hacer la predicción a futuro
forecast_future <- forecast(modelo_final, xreg = future_xreg, h = h)
pred_df <- data.frame(
  Fecha = seq(from = max(monthly_data$Fecha) + 30, by = "month", length.out = 120),
  Prediccion = as.numeric(forecast_future$mean),
  IC_Lower_80 = as.numeric(forecast_future$lower[,1]),
  IC_Upper_80 = as.numeric(forecast_future$upper[,1]),
  IC_Lower_95 = as.numeric(forecast_future$lower[,2]),
  IC_Upper_95 = as.numeric(forecast_future$upper[,2])
)

# Datos históricos
hist_data <- data.frame(Fecha = monthly_data$Fecha, Temperatura = monthly_data$MeanTemp)

# Datos combinados para el suavizado lineal
combined_data <- rbind(
  hist_data,
  data.frame(Fecha = pred_df$Fecha, Temperatura = pred_df$Prediccion)
)

# Gráfico
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
    title = "Predicción SARIMAX de Temperatura Media",
    x = "Fecha", y = "Temperatura Media (°C)"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardar resultados en Excel
write.xlsx(pred_df, "Prediccion_sarimax.xlsx")
cat("Predicciones SARIMAX guardadas como 'Prediccion_sarimax.xlsx'\n")

# -----------------------------------------------------
# ANÁLISIS DE LA ESTABILIDAD DEL MODELO EN EL TIEMPO (2012–2024)
# -----------------------------------------------------
# Dividir el test en bloques de 3 años
real_vs_pred_presion$Year <- format(real_vs_pred_presion$Fecha, "%Y")

# Crear bloques: 2012-2014, 2015-2017, etc.
real_vs_pred_presion$Bloque <- cut(as.numeric(real_vs_pred_presion$Year),
                                   breaks = c(2011, 2014, 2017, 2020, 2023, 2026),
                                   labels = c("2012–2014", "2015–2017", "2018–2020", "2021–2023", "2024"))

# Calcular RMSE por bloque
errores_bloques <- real_vs_pred_presion %>%
  group_by(Bloque) %>%
  summarise(
    RMSE = rmse(Real, Predicho),
    MAE = mae(Real, Predicho)
  )

# Mostrar tabla de errores por bloque
print(errores_bloques)

# Gráfico de RMSE por bloque temporal
ggplot(errores_bloques, aes(x = Bloque, y = RMSE, group = 1)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point(size = 3, color = "firebrick") +
  labs(
    title = "Evolución del Error RMSE en el tiempo (Test)",
    x = "Bloque Temporal", y = "RMSE"
  ) +
  theme_minimal(base_size = 14)
