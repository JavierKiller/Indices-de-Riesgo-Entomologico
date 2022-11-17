library(tidyverse)


#cargar base de datos de lectura de ovitrampas
dfov <- read.csv(file = "enero/DescargaLecturase26_10.txt", header = TRUE, sep = "\t", fileEncoding = 'utf-16')
head(dfov)

#cargar base de datos de Estudio entomologico
dfee0 <- read.csv(file = "enero/DescargaEntomologicoe26_10.txt", header = TRUE, sep = "\t", fileEncoding = 'utf-16')
head(dfee0)

dfee <- dfee0 %>%
  mutate()
dfee
names(dfee)
str(dfee)

#convertir el tipo de dato de semana epidemiologica de numero a caracter
dfee$Semana.Epidemiologica <- as.character(dfee$Semana.Epidemiologica) 


#Filtrar datos por tipo de estudio 

#Tipo de Estudio(Encuesta) 

dfeee<-dfee %>% filter(Tipo.de.Estudio == 'Encuesta')
dfeee


#Filtrar por localidad 
#Obregon
dfeeeobregon<-dfeee %>% filter(Localidad == '0001 CIUDAD OBREGÓN')

#sumar datos por Semana epidemiologica
dfeeeobregon %>% 
  group_by(Semana.Epidemiologica) %>% colSums(na.rm = TRUE)
dfeeeobregon %>% mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))


#Guaymas
dfeeeguaymas<-dfeee %>% filter(Localidad == '0001 HEROICA GUAYMAS')
#sumar datos por Semana epidemiologica
dfeeeguaymas %>% 
  group_by(Semana.Epidemiologica) %>% colSums(na.rm = TRUE)
dfeeeguaymas <- dfeeeguaymas %>% mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))


#Hermosillo
dfeeehermosillo<-dfeee %>% filter(Localidad == '0001 HERMOSILLO')
#sumar datos por Semana epidemiologica
dfeeehermosillo %>% 
  group_by(Semana.Epidemiologica) %>% colSums(na.rm = TRUE)
dfeeehermosillo <- dfeeehermosillo %>% mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))


#Navojoa
dfeeenavojoa<-dfeee %>% filter(Localidad == '0001 NAVOJOA')
#sumar datos por Semana epidemiologica
dfeeenavojoa %>% 
  group_by(Semana.Epidemiologica) %>% colSums(na.rm = TRUE)
dfeeenavojoa <- dfeeenavojoa %>% mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))




#Tipo de Estudio(Verificacion)
dfeev<-dfee %>% filter(Tipo.de.Estudio == 'Verificacion')
dfeev

#Filtrar por localidad 
#Obregon
dfeevobregon<-dfeev %>% filter(Localidad == '0001 CIUDAD OBREGÓN')
#sumar datos por Semana epidemiologica
dfeevobregon %>% 
  group_by(Semana.Epidemiologica) %>% colSums(na.rm = TRUE)
dfeevobregon <- dfeevobregon %>% mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))



#Guaymas
dfeevguaymas<-dfeev %>% filter(Localidad == '0001 HEROICA GUAYMAS')

#sumar datos por Semana epidemiologicadfeevguaymas %>% 
  group_by(Semana.Epidemiologica) %>% colSums(na.rm = TRUE)
dfeevguaymas <- dfeevguaymas %>% mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))



#Hermosillo
dfeevhermosillo<-dfeev %>% filter(Localidad == '0001 HERMOSILLO')
#sumar datos por Semana epidemiologica
dfeevhermosillo %>% 
  group_by(Semana.Epidemiologica) %>% colSums(na.rm = TRUE)
dfeevhermosillo <- dfeevhermosillo %>% mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))


#Navojoa
dfeevnavojoa<-dfeev %>% filter(Localidad == '0001 NAVOJOA')
#sumar datos por Semana epidemiologica
dfeevnavojoa %>% 
  group_by(Semana.Epidemiologica) %>% colSums(na.rm = TRUE)
dfeevnavojoa <- dfeevnavojoa %>% mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
