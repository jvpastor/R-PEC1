library(readr)
titanic <- read_csv("C:/Users/josev/Desktop/Máster en Business Intelligence y Data Science/Entorno de análisis de datos (R)/titanic.csv")
View(titanic)
# 1. Lectura del fichero titanic.csv
totalSurvived <- sum(titanic$Survived)
totalPassenger <- max(titanic$PassengerId)
totalSurvived/totalPassenger
# 2. Cálculo del porcentaje de supervivientes
sum(is.na.data.frame(titanic$PassengerId)) / totalPassenger
# 3. Cálculo del porcentaje de missing values en los siguientes atributos:
sum(is.na.data.frame(titanic$Pclass)) / totalPassenger
sum(is.na.data.frame(titanic$Name)) / totalPassenger
sum(is.na.data.frame(titanic$Sex)) / totalPassenger
sum(is.na.data.frame(titanic$Age)) / totalPassenger
sum(is.na.data.frame(titanic$SibSp)) / totalPassenger
sum(is.na.data.frame(titanic$Parch)) / totalPassenger
sum(is.na.data.frame(titanic$Ticket)) / totalPassenger
sum(is.na.data.frame(titanic$Fare)) / totalPassenger
sum(is.na.data.frame(titanic$Cabin)) / totalPassenger
sum(is.na.data.frame(titanic$Embarked)) / totalPassenger
titanic$Cabin <- NULL
# 4. Eliminar la variable Cabin
median(titanic$Age, na.rm = TRUE)
# 5. Cálculo de la mediana de la edad
aggregate(Survived~Sex, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 6. Cálculo de la probabilidad de supervivientes en base al sexo
# La conclusión que se puede sacar es que a la hora de utilizar botes y salvavidas,
# se priorizaban las mujeres y los niños
aggregate(Survived~Age, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 7. Cálculo de la probabilidad de supervivientes en base a la edad
# No es para nada fácil la interpretación de los datos debido a que los toma a partir
# de cada uno de los valores de edad y solo en caso de que se repita un valor,
# aparece una probabilidad distinta a 0 o 1
titanic$decade <- '80+'
titanic$decade [titanic$Age < 80 & titanic$Age >= 70] <- '70-80'
titanic$decade [titanic$Age < 70 & titanic$Age >= 60] <- '60-70'
titanic$decade [titanic$Age < 60 & titanic$Age >= 50] <- '50-60'
titanic$decade [titanic$Age < 50 & titanic$Age >= 40] <- '40-50'
titanic$decade [titanic$Age < 40 & titanic$Age >= 30] <- '30-40'
titanic$decade [titanic$Age < 30 & titanic$Age >= 20] <- '20-30'
titanic$decade [titanic$Age < 20 & titanic$Age >= 10] <- '10-20'
titanic$decade [titanic$Age < 10] <- '10-'
# Creación de la variable decade para agrupar la edad por décadas
aggregate(Survived~decade, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 8. Cálculo de la probabilidad de supervivientes en base a la edad agrupando por décadas
# Se puede ver cómo casi dos terceras partes de los niños menores de 10 años, sobrevivieron;
# los mayores de 60 años fue el rango de edad con menos supervivientes (menos de un tercio de ellos),
# en el resto de rangos de edad el porcentaje de supervivencias estuvo alrededor del 40%
aggregate(Survived~Pclass, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 9. Cálculo de la probabilidad de supervivientes en base a la clase del billete
# Se puede concluir que hay una correlación entre el precio del billete y la probabilidad de supervivencia
aggregate(Survived~Pclass + Sex, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 10. Cálculo de la probabilidad de supervivientes en base a la clase del billete y el sexo
# Se puede concluir que la práctica totalidad de mujeres de con billete de primera y segunda clase,
# tan solo la mitad de las mujeres de con billete de tercera clase sobrevivieron;
# mientras que tan solo un tercio de los hombres con billete de primera clase sobrevivió,
# para el resto de hombre, la probabilidad de supervivencia fue más bien escasa
titanic$familysize <- titanic$SibSp + titanic$Parch
# 11. Creación de la variable con el número total de parientes por pasajero
titanic$sigleton <- titanic$familysize == 0
# Creación de la variable que indica las personas que viajaban sin parientes
aggregate(Survived~titanic$sigleton, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 12. Cálculo de la probabilidad de supervivientes en base a que viajasen
# sin parientes (TRUE) o con parientes (FALSE)
# Se puede concluir una mayor supervivencia para los pasajeros que viajaban con algún familiar
# que con respecto a los que viajaban solos (50-30)
matrix1 <- table(titanic$familysize, titanic$Pclass)
# 13. Creación de matrix1 para la comprobación del número de familiares de los pasajeros con la clase del billete

