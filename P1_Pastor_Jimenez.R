library(readr)
titanic <- read_csv("C:/Users/josev/Desktop/M�ster en Business Intelligence y Data Science/Entorno de an�lisis de datos (R)/titanic.csv")
View(titanic)
# 1. Lectura del fichero titanic.csv
totalSurvived <- sum(titanic$Survived)
totalPassenger <- max(titanic$PassengerId)
totalSurvived/totalPassenger
# 2. C�lculo del porcentaje de supervivientes
sum(is.na.data.frame(titanic$PassengerId)) / totalPassenger
# 3. C�lculo del porcentaje de missing values en los siguientes atributos:
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
# 5. C�lculo de la mediana de la edad
aggregate(Survived~Sex, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 6. C�lculo de la probabilidad de supervivientes en base al sexo
# La conclusi�n que se puede sacar es que a la hora de utilizar botes y salvavidas,
# se priorizaban las mujeres y los ni�os
aggregate(Survived~Age, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 7. C�lculo de la probabilidad de supervivientes en base a la edad
# No es para nada f�cil la interpretaci�n de los datos debido a que los toma a partir
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
# Creaci�n de la variable decade para agrupar la edad por d�cadas
aggregate(Survived~decade, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 8. C�lculo de la probabilidad de supervivientes en base a la edad agrupando por d�cadas
# Se puede ver c�mo casi dos terceras partes de los ni�os menores de 10 a�os, sobrevivieron;
# los mayores de 60 a�os fue el rango de edad con menos supervivientes (menos de un tercio de ellos),
# en el resto de rangos de edad el porcentaje de supervivencias estuvo alrededor del 40%
aggregate(Survived~Pclass, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 9. C�lculo de la probabilidad de supervivientes en base a la clase del billete
# Se puede concluir que hay una correlaci�n entre el precio del billete y la probabilidad de supervivencia
aggregate(Survived~Pclass + Sex, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 10. C�lculo de la probabilidad de supervivientes en base a la clase del billete y el sexo
# Se puede concluir que la pr�ctica totalidad de mujeres de con billete de primera y segunda clase,
# tan solo la mitad de las mujeres de con billete de tercera clase sobrevivieron;
# mientras que tan solo un tercio de los hombres con billete de primera clase sobrevivi�,
# para el resto de hombre, la probabilidad de supervivencia fue m�s bien escasa
titanic$familysize <- titanic$SibSp + titanic$Parch
# 11. Creaci�n de la variable con el n�mero total de parientes por pasajero
titanic$sigleton <- titanic$familysize == 0
# Creaci�n de la variable que indica las personas que viajaban sin parientes
aggregate(Survived~titanic$sigleton, data = titanic, FUN = function(x) {sum(x)/length(x)})
# 12. C�lculo de la probabilidad de supervivientes en base a que viajasen
# sin parientes (TRUE) o con parientes (FALSE)
# Se puede concluir una mayor supervivencia para los pasajeros que viajaban con alg�n familiar
# que con respecto a los que viajaban solos (50-30)
matrix1 <- table(titanic$familysize, titanic$Pclass)
# 13. Creaci�n de matrix1 para la comprobaci�n del n�mero de familiares de los pasajeros con la clase del billete

