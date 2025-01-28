#Indicaciones:
  
# a. Crea un proyecto de Quarto.
# Se crea proyecto en quarto con el nombre "Reto8_Proyecto_aplicado

# lectura de datos:
df_nacidos<- read.csv("/Users/danielmerkdo./Documents/Curso R-Studio/Nacidos_Hospital_San_Juan_de_Dios_Rionegro-Ant._20241006.csv")

# b. Identifica el tipo de variables que la conforman: deja plasmado en el documento el tipo de variables que encontraste.

# Tipo de variables
str(df_nacidos)
tipo_variables <- sapply(df_nacidos, class)
tipo_variables

# c. Lee y explora la base datos (formato .csv): puede hacerse con una función o con las funcionalidades de RStudio.
head(df_nacidos)
# Tamaño del data set
dim(df_nacidos)
# Nombre de las variables
names(df_nacidos)
# Encontrar datos nulos
datos_nulos <- sapply(df_nacidos, function(x) sum(is.na(x)))
datos_nulos


# d. Identifica el tipo de variables que la conforman: deja plasmado en el documento el tipo de variables que encontraste.

tipo_variables <- sapply(df_nacidos, class)
tipo_variables


# e. Prepara el marco de datos:
#Asegúrate de que R haya leído el tipo de variable correctamente; en caso contrario, cámbialo.

#Identifica si hay valores faltantes.
summary(df_nacidos)
# Encontrar datos nulos
datos_nulos <- sapply(df_nacidos, function(x) sum(is.na(x)))
datos_nulos
#Renombra las columnas Peso (gramos) y Talla (centímetros).
library(dplyr)

df_nacidos2 <- df_nacidos %>%
  rename("Centimetros" = "TALLA..Centímetros.", "Gramos"= "PESO..Gramos." )
names(df_nacidos2)



# RETO 9
# lectura de datos:
df_nacidos<- read.csv("/Users/danielmerkdo./Documents/Curso R-Studio/Nacidos_Hospital_San_Juan_de_Dios_Rionegro-Ant._20241006.csv")

# a. Haz un análisis descriptivo con las medidas estudiadas. Como mínimo se requiere:
#Resumen del marco de datos (función summary)
summary(df_nacidos2)


#Media, mediana y desviación estándar para las variables cuantitativas: peso, talla, tiempo de gestación y consultas prenatales.
# Función para calcular media, mediana y desviación estándar

calcular_estadisticos <- function(df_nacidos2) {
  # Seleccionar solo las variables cuantitativas
  variables_cuantitativas <- df_nacidos2[, c("Gramos", "Centimetros", "TIEMPO.DE.GESTACIÓN", "CONSULTAS.PRENATALES")]
  
  # Calcular estadísticas para cada variable
  resultados <- sapply(variables_cuantitativas, function(x) {
    # Manejo de valores NA
    x <- x[!is.na(x)] #Elimina los NA's antes del cálculo
    if (length(x)>0){ #verifica que hayan datos después de eliminar NA's
      c(media = mean(x), mediana = median(x), desviacion_estandar = sd(x))
    } else {
      c(media = NA, mediana = NA, desviacion_estandar = NA) #si no hay datos después de eliminar NA's
    }
  })
  
  # Transponer la matriz de resultados para una mejor visualización
  resultados <- t(resultados)
  
  return(resultados)
}

# Llamar a la función con tu dataframe
estadisticos_df_nacidos2 <- calcular_estadisticos(df_nacidos2)

# Mostrar los resultados
print(estadisticos_df_nacidos2)
# Llamar a la función con tu dataframe
estadisticos_df_nacidos2 <- calcular_estadisticos(df_nacidos2)

# Mostrar los resultados
print(estadisticos_df_nacidos2)
  

  




#Moda para las variables cualitativas: sexo y multiplicidad de embarazo.

# Función para calcular la moda 
calcular_moda<- function(x) {
  x <- x[!is.na(x)] # Eliminar NA's
  frecuencias <- table(x)
  max_frecuencia <- max(frecuencias)
  moda <- names(frecuencias)[frecuencias == max_frecuencia]
  return(moda)
}

# Calcular la moda para cada variable
moda_sexo <- calcular_moda(df_nacidos2$SEXO)
moda_multiplicidad <- calcular_moda(df_nacidos2$MULTIPLICIDAD.EMBARAZO)

# Mostrar los resultados
cat("La moda del sexo es:", paste(moda_sexo, collapse = ", "), "\n")
cat("La moda de la multiplicidad del embarazo es:", paste(moda_multiplicidad, collapse = ", "), "\n")




#Medida agrupando una variable (Group by): cantidad de nacimientos por área nacimiento.

# Calcular la cantidad de nacimientos por área de nacimiento
nacimientos_por_area <- df_nacidos2 %>%
  group_by(AREA.NACIMIENTO) %>%
  summarise(Cantidad_de_nacimientos = n())
print(nacimientos_por_area)



#Medida con información filtrada (Filter): peso y talla promedio de los niños con siete o más consultas prenatales y cantidad de niños con padres mayores a 35 años.

# Calcular el promedio de Gramos y Centímetros para niños con 7 o más consultas prenatales
promedios <- df_nacidos2 %>%
  filter(CONSULTAS.PRENATALES >= 7) %>%
  summarise(Promedio.Gramos = mean(Gramos, na.rm = TRUE), 
            Promedio.Centimetros = mean(Centimetros, na.rm = TRUE))

print("Promedios para niños con 7 o más consultas prenatales:")
print(promedios)



# Calcular la cantidad de niños con padres mayores a 35 años
niños_padres_mayores_35 <- df_nacidos2 %>%
  filter(EDAD.PADRE > 35 | EDAD.MADRE >35) %>%
  summarise(Cantidad = n())

print("Cantidad de niños con al menos un padre mayor a 35 años:")
print(niños_padres_mayores_35)


# b. Haz un análisis descriptivo a partir de gráficos. Como mínimo se debe utilizar lo siguiente:
#Gráfico de barras: nacimientos registrados por sexo y tipo de parto 
library(ggplot2)


df_nacidos2$SEXO
# Crear el gráfico de barras
grafico <- ggplot(df_nacidos2, aes(x = SEXO, fill = TIPO.PARTO)) +
  geom_bar(position = "dodge") +  
  labs(title = "Conteo de Nacimientos por Sexo y Tipo de Parto",
       x = "Sexo",
       y = "Conteo",
       fill = "Tipo de Parto") +
  theme_bw()  
print(grafico)


#Gráfico de barras:  nacimientos registrados por área de nacimiento y tipo de parto.


grafico <- ggplot(df_nacidos2, aes(x = AREA.NACIMIENTO, fill = TIPO.PARTO)) +
  geom_bar(position = "dodge") +  
  labs(title = "Conteo de Nacimientos por Sexo y Tipo de Parto",
       x = "Sexo",
       y = "Conteo",
       fill = "Tipo de Parto") +
  theme_bw()  
print(grafico)





#Gráfico de dispersión: variable peso vs. talla.

# Crear el gráfico de dispersión
grafico_dispersion <- ggplot(df_nacidos2, aes(x = Gramos, y = Centimetros)) +
  geom_point() +  
  labs(title = "Gráfico de Dispersión: Gramos vs. Centimetros",
       x = "Gramos",
       y = "Centimetros") +
  theme_bw() 
print(grafico_dispersion)


# Calcular la correlación coeficiente de Pearson  entre Gramos y Centímetros
correlacion <- cor(df_nacidos2$Gramos, df_nacidos2$Centimetros, use ="complete.obs")
cat("El coeficiente de correlación entre Gramos y Centimetros es:", correlacion, "\n")

# c. Redacta las conclusiones que sean pertinentes.

# Conclusiones:

#Durante este proyecto contamos con un data set que contiene 1821 registros y 14 variables que corresponden a nacimientos en el hospital San Juan de Rionegro Ant en el año 2022. En este data set se registraron 1821 nacimientos caracterizándolos a través de variables (tipo integer) como; año, gramos, centímetros, fecha nacimiento, tiempo de gestación, consultas prenatales, edad madre y edad padre. También, con variables (tipo character) como; departamento, municipio, área nacimiento, sexo, fecha nacimiento, tipo parto, multiplicidad embarazo. Dichas variables, muy importantes para describir la natalidad en el municipio de Rionegro y en dicho hospital. Haciendo un análisis exploratorio de los datos podemos decir que el peso de los recién nacidos presento un mínimo de 700 gramos, una media de 3093 y un máximo de 4610 gramos. Con respecto a la talla de los neonatos encontramos, un mínimo de 32 centímetros, una media de 48 y un máximo de 55 centímetros. Ahora, el tiempo de gestación mínimo se registró en 37 semanas, una media de 38.75 y una máxima de 41 semanas con consultas prenatales de 0 como mínimo, 7 como la media y un máximo de 18 consultas. Entre tanto, las edades de los padres registraron como 14 años como mínimo, 25 años como la media y una edad máxima de 45 en la madre y en el padre una edad mínima de 16 años, una edad media de 29.45 y una máxima de 77 años.
#Por otra parte, al análisis de datos nulos se encontraron un total de 13 en la variable edad padre. En cuanto a la desviación estándar, se analizaron variables importantes y significativas; la variable gramos con desviación estándar de 363.89 lo que nos indica que los datos de esta variable están muy dispersos y nos puede hacer ruido en el análisis, para las variables; centímetros, tiempo de gestación y consultas prenatales se encontraron valores muy bajos en su desviación 1.91 , 1.08 y 2.41 respectivamente indicándonos, que estos datos se agrupan bien alrededor del promedio de los mismos. Para las variables categóricas sexo y multiplicidad embarazo se encontró que la moda de estas estuvo en Masculino y Simple, es decir, el sexo masculino predominó en los nacimientos y las maternas primerizas fueron quienes más dieron a luz. Así mismo, estos nacimientos se registraron en dos áreas de nacimiento con un total de 1818 en la cabecera municipal y solo 3 en el área rural y tanto, padre como madre de los recién nacidos mayores de 35 años llegó a la cifra de 405.

#Los partos que se practicaron el hospital San Juan fueron 3; cesárea con más de 300 partos de neonatos femeninos y casi 400 masculinos mientras, que el parto espontáneo fue el que más registros tuvo en ambos sexos con una cifra superior a 500 partos y el parto instrumentado tuvo unas cifras muy bajas en ambos sexos.
#Finalmente, se analizó el tallaje y el peso de los recién nacidos a través de un gráfico de dispersión, existe una correlación positiva entre el peso (en gramos) y la longitud (en centímetros) al nacer, a medida que aumenta el peso al nacer, tiende a aumentar también la longitud. Esta relación no es perfecta, ya que hay variabilidad en los datos, pero la tendencia es clara. La mayoría de los datos se concentran en un rango específico de peso 2300 y 4000 gramos y de longitud entre 45 y 52 centímetros también, se observa una concentración de puntos con inclinación a la derecha, indicando asociación entre las dos variables. Si bien existe una correlación positiva, no parece ser una relación perfectamente lineal. Por ello, calculamos el índice de correlación de Pearson el cual, nos entrega una correlación del 66% que si bien, es positiva no representa una correlación fuerte y se considera moderada. Además, se observó valores atípicos que pueden ser interesante para investigar a fondo ya que podrían corresponder a casos especiales o errores en la medición, algunos bebes son más grandes por su peso y viceversa. 
















