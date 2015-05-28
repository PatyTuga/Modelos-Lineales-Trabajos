##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre:PATRICIA GUERRERO 


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()

data = read.table("data.txt", header = TRUE,dec=",", sep="\t")
summary(data)
str(data)


# 2.2 Calcular el mi???nimo, la media, el máximo de la variable Edad
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE

edad <- data[,1]

mean(edad,na.rm=TRUE)
min(edad,na.rm=TRUE)
max(edad, na.rm=TRUE)
atip <- max(data[,1],na.rm=TRUE)
atip
# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

data_f <- subset(data, subset=data[,"Genero"]=="Femenino")
table(data[,"Genero"])
table(data_f[,"Genero"])
#Hemos encontardo 6183 femenino y 12302 son masculino

# 2.4 Encontrar la Edad m????nima, media, máxima de los sujetos que Si son dependientes.

data_p <- subset(data, subset=data[,"Dependiente"]=="Si")
max(data_p[,1],na.rm=TRUE)
min(data_p[,1],na.rm=TRUE)
mean(data_p[,1],na.rm=TRUE)

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()
# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

tipos <- numeric(ncol(data))
clase <- numeric(ncol(data))
for (i in 1:ncol(data)){
  tipos[i] <- typeof(data[,i])
  clase[i] <- class(data[,i])
}
tipos
clase

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para n factor no es posible obtener la media debido a que 
# éstos representan variables

tipos <- numeric(ncol(data))

for (i in 1:ncol(data))
{
  tipos[i] <- is.numeric(data[,i])
}
tipos_TF<-as.logical(tipos)

data_num<-data[,c(TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)]
data_num
# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()


for (i in 1:ncol(data))
{
  data_na[i]<-sum(is.na(data[,i]))
}
data_na

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

data_e <- subset(data, subset=data[,"Edad"]>40)

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

data_v <- subset(data, subset=data[,"Vivienda"]=="Propia")

# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

data_c <- subset(data, subset=data[,"Cargas"]>1)

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

data_s <- subset(data, subset=data[,"Deuda"]>=500)
data_s1<-subset(data_s,subset=data_s[,"Dias_Atraso"]>8 )
data_s1

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

data_sc <- subset(data, subset=data[,"Score"]>=900)
data_sc1<-subset(data_sc,subset=data_sc[,"Edad"]>=35)
data_sc2<-subset(data_sc1,subset=data_sc1[,"Numero_TC"]>3)
data_sc2
# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(edad,col = "red")
# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

boxplot(edad,col = "green")
