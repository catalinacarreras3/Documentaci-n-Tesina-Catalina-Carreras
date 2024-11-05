install.packages("dplyr")
install.packages("zoo")
install.packages("tidyr")
install.packages("purrr")
library(zoo)
library(dplyr)
library(tidyr)
library(purrr)

database_full <- read.csv("C:\\Users\\catal\\Downloads\\database_final1.csv", header = TRUE, sep = ",")
head(data, n= 30)

database <- database_full[, !names(database_full) %in% c("PRODUCTO_VENTA", "VENTA_VENTA", "SISTEMA","PRODUCTO_DESCUENTO_CUPON", "CODIGOCUPON",
                                        "PRODUCTO_DESCUENTO_PRIMERA_COMPRA", "DESCUENTO_PEDIDO", "COMENTARIO",
                                        "PEDIDOCAMPOEXTRA1", "PEDIDOCAMPOEXTRA2", "PEDIDOCAMPOEXTRA3",
                                        "PEDIDOCAMPOEXTRA4", "PEDIDOCAMPOEXTRA5", "PEDIDOCAMPOEXTRA6",
                                        "PEDIDOCAMPOEXTRA7", "PEDIDOCAMPOEXTRA8", "PEDIDOCAMPOEXTRA9",
                                        "PEDIDOCAMPOEXTRA10", "PRODUCTO_DESCUENTO", "PRODUCTO_PRECIOTOTAL",
                                        "RETORNO", "COSTO", "ATENDIDAS", "PRODUCTO_BON", "PRODUCTO_BON_DIS",
                                        "PRODUCTO_COMBOID", "PRODUCTO_CODIGO", "DROGUERIAID", "PRODUCTO_AGRUPADOR2",
                                        "MEDICOID", "ETIQUETA_MEDICO", "ETIQUETA_APM", "ESTADO")]

database <- database %>%
  mutate(
    PRODUCTO_UNIDADES = as.numeric(PRODUCTO_UNIDADES),
    PRODUCTO_PRECIO = as.numeric(PRODUCTO_PRECIO),
    PRODUCTO_PRECIOBRUTO = as.numeric(PRODUCTO_PRECIOBRUTO),
    FECHA = as.yearmon(FECHA, format = "%Y-%m")
  )

fecha_menor <- min(database$FECHA, na.rm = TRUE)
fecha_mayor <- max(database$FECHA, na.rm = TRUE)


factor_ajuste <- c(17.11141667, 16.51681146, 15.76031628, 15.13959296, 14.65594672, 14.20149876, 
                              13.78786288, 13.45157354, 12.99668941, 12.55718783, 12.25091496, 11.80242289, 
                              11.35940605, 10.84948047, 10.16821038, 9.592651299, 9.127165841, 8.667773828, 
                              8.070552913, 7.542572815, 7.10223429, 6.681311655, 6.369219881, 6.060152123, 
                              5.717124644, 5.363156327, 4.979718038, 4.593835828, 4.261443254, 4.020229485, 
                              3.781965649, 3.364738122, 2.98557065, 2.756759603, 2.443935818, 1.947359217, 
                              1.614725719, 1.426436148, 1.285077611, 1.18113751, 1.13352928, 1.08368, 1.042, 1)


num_meses <- 44 
fechas_ajuste <- as.yearmon("2021-01") + seq(0, num_meses - 1) / 12

ajustes <- data.frame(
  FECHA = fechas_ajuste,
  factor_ajuste  = as.numeric(factor_ajuste)
)

database <- database %>%
  left_join(ajustes,by="FECHA")
str(database)

database <- database %>%
  mutate(
    PRODUCTO_PRECIO_AJUSTADO = PRODUCTO_PRECIO * factor_ajuste,
    PRODUCTO_PRECIOBRUTO_AJUSTADO = PRODUCTO_PRECIOBRUTO * factor_ajuste
  )

summary(database)

resumen_pedidos <- database %>%
  group_by(PEDIDOID, FECHA, DROGCODIGO) %>%
  summarise(
    total_unidades = sum(PRODUCTO_UNIDADES, na.rm = TRUE),
    total_precio_bruto = sum(PRODUCTO_PRECIOBRUTO_AJUSTADO, na.rm = TRUE),
    cantidad_productos = n_distinct(PRODUCTO_NOMBRE)
  ) %>%
  ungroup()

precios_mas_altos <- resumen_pedidos %>%
  arrange(desc(total_precio_bruto)) %>%
  slice_head(n = 3) %>%
  pull(total_precio_bruto)

resumen_pedidos <- resumen_pedidos %>%
  filter(!total_precio_bruto %in% precios_mas_altos)

precios_mas_bajos <- resumen_pedidos %>%
  arrange(total_precio_bruto) %>%
  slice_head(n = 100) %>%
  pull(total_precio_bruto)

resumen_pedidos <- resumen_pedidos %>%
  filter(!total_precio_bruto %in% precios_mas_bajos)


summary(resumen_pedidos)

hist(resumen_pedidos$total_precio_bruto, 
     main = "Distribución de Precios Totales Brutos por Pedido", 
     xlab = "Total Precio Bruto", 
     ylab = "Frecuencia", 
     breaks=100,
     ylim = c(0,4000))

# Definir la fecha de inicio para calcular la diferencia en meses

fecha_inicio <- as.yearmon("2021-01")

# Crear el resumen de pedidos por droguería
resumen_droguerias <- resumen_pedidos %>%
  group_by(DROGCODIGO) %>%
  summarise(
    # Cantidad de compras
    cantidad_compras = n(),
    
    # Fecha de la última compra en formato yearmon
    ultima_compra = max(FECHA, na.rm = TRUE) %>% as.yearmon(),
    
    # Meses desde la primera fecha hasta la última compra
    meses_desde_inicio = as.numeric((ultima_compra - fecha_inicio) * 12),
    
    # Frecuencia de compra anual ajustada
    frecuencia_compra = (cantidad_compras / (meses_desde_inicio+1))*12,
    
    # Recencia en meses desde la última compra
    meses_desde_ultima_compra = as.numeric((as.yearmon(Sys.Date()) - ultima_compra) * 12),
    
    # Monto promedio de compra
    monto_promedio = mean(total_precio_bruto, na.rm = TRUE),
    
    # Todos los montos de compra como lista
    montos_compras = list(total_precio_bruto)
  ) %>%
  ungroup() %>%
  
  # Columna que indica si la última compra es anterior a agosto de 2023
  mutate(
    abandono  = ifelse(ultima_compra < as.yearmon("2023-08"), 1, 0)
  )


# Reorganizar y eliminar columnas innecesarias
resumen_droguerias <- resumen_droguerias %>%
  select(
    DROGCODIGO,
    cantidad_compras,
    frecuencia_compra,
    ultima_compra,
    meses_desde_ultima_compra,
    monto_promedio,
    abandono,
    montos_compras
  )

summary(resumen_droguerias)


# Calcular el número total de filas en el dataframe
n_total <- nrow(resumen_droguerias)

# Crear el subconjunto de entrenamiento (todas las filas excepto las últimas 200)
entrenamiento <- resumen_droguerias[1:5000, ]

# Crear el subconjunto de prueba (las últimas 200 filas)
prueba <- resumen_droguerias[5000:n_total, ]

# Convertir la variable de respuesta abandono a formato numérico
resumen_droguerias$abandono <- as.numeric(as.character(resumen_droguerias$abandono))

modelo_lineal <- lm(abandono ~ frecuencia_compra + meses_desde_ultima_compra + monto_promedio, data = entrenamiento)
summary(modelo_lineal)

residuos <- residuals(modelo_lineal)
hist(residuals(modelo_lineal), 
     main = "Distribución de los Residuos del Modelo de Regresión",
     xlab = "Residuos", 
     ylab = "Frecuencia", 
     col = "lightblue", 
     breaks=30)
shapiro_test <- shapiro.test(residuos)

# Ver el resultado
shapiro_test

predicciones <- predict(modelo_lineal, newdata = prueba)


# Agregar las predicciones ajustadas al conjunto de prueba
prueba <- prueba %>%
  mutate(predicciones = predicciones) 
# Crear gráfico de dispersión para contrastar predicciones con resultados reales
plot(prueba$abandono, prueba$predicciones,
     main = "Comparación de Predicciones y Resultados Reales de Abandono",
     xlab = "Abandono Real (0 = No, 1 = Sí)",
     ylab = "Probabilidad de Abandono Predicha",
     col = "blue", pch = 16)

# Agregar una línea de referencia en 0.5 para ver el umbral de decisión
abline(h = 0.5, col = "red", lty = 2)


# Calcular las ventas anuales en el dataframe ventas_anuales
# Calcular las ventas anuales en el dataframe ventas_anuales con formato sin notación científica
ventas_anuales <- database %>%
  mutate(anio = as.integer(format(FECHA, "%Y"))) %>%
  group_by(DROGCODIGO) %>%
  summarise(
    ventas_2021 = as.numeric(format(sum(PRODUCTO_PRECIOBRUTO_AJUSTADO[anio == 2021], na.rm = TRUE))),
    ventas_2022 = as.numeric(format(sum(PRODUCTO_PRECIOBRUTO_AJUSTADO[anio == 2022], na.rm = TRUE))),
    ventas_2023 = as.numeric(format(sum(PRODUCTO_PRECIOBRUTO_AJUSTADO[anio == 2023], na.rm = TRUE)))
  )

# Unir las ventas anuales al dataframe prueba
prueba <- prueba %>%
  left_join(ventas_anuales, by = "DROGCODIGO")


# Definir la función para predecir ventas usando regresión lineal
predecir_ventas_lineal <- function(ventas) {
  # Crear un dataframe de años y ventas
  data <- data.frame(
    anio = c(2021, 2022, 2023),
    ventas = ventas
  )
  
  # Ajustar el modelo de regresión lineal
  modelo <- lm(ventas ~ anio, data = data)
  # Proyectar el valor para 2024
  prediccion <- predict(modelo, newdata = data.frame(anio = 2024))
  
  # Devolver 0 si la predicción es negativa, de lo contrario devolver la predicción
  if (as.numeric(prediccion) < 0) {
    return(0)
  } else {
    return(as.numeric(prediccion))
  }
}

# Aplicar la función de predicción a cada fila del dataset prueba
prueba <- prueba %>%
  mutate(
    prediccion_2024 = pmap_dbl(
      list(ventas_2021, ventas_2022, ventas_2023),
      ~ predecir_ventas_lineal(c(...))
    )
  )

# Ver el resultado con la proyección de 2024
head(prueba)



# Definir la tasa de descuento
tasa_descuento <- 0.04

# Calcular el valor actual para las predicciones de 2024, aplicando la tasa de descuento y la probabilidad de supervivencia
prueba <- prueba %>%
  mutate(
    valor_actual_2024 = ifelse(
      prediccion_2024 > 0, 
      prediccion_2024 / ((1 + tasa_descuento) * (1 - abandono)), 
      0
    )
  )

# Ver el resultado con el valor actual de 2024
head(prueba)

