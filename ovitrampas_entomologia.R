library(tidyverse)


#cargar base de datos de lectura de ovitrampas
dfov <- read.csv(file = "enero/DescargaLecturase26_10.txt", header = TRUE, sep = "\t", fileEncoding = 'utf-16')
head(dfov)

#cargar base de datos de Estudio entomologico
dfee0 <- read.csv(file = "enero/DescargaEntomologicoe26_10.txt", header = TRUE, sep = "\t", fileEncoding = 'utf-16')
head(dfee)


str(dfee)

#convertir el tipo de dato de semana epidemiologica de numero a caracter
dfee$Semana.Epidemiologica <- as.character(dfee$Semana.Epidemiologica) 
#dfee$Sector <- as.character(dfee$Sector)
#dfee$Manzanas.exploradas <- as.character(dfee$Manzanas.exploradas)

#Filtrar datos por tipo de estudio 

#Tipo de Estudio(Encuesta) 

dfeee<-dfee %>% filter(Tipo.de.Estudio == 'Encuesta')
dfeee


#Filtrar por localidad 
#Obregon
dfeeeobregon<-dfeee %>% filter(Localidad == '0001 CIUDAD OBREGÓN')

#sumar datos por Semana epidemiologica


dfeeeobregon<-dfeeeobregon %>%
  group_by(Semana.Epidemiologica) %>% summarise(across(where(is.numeric), .fns = sum)) %>%
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
dfeeeobregon

#Guaymas
dfeeeguaymas<-dfeee %>% filter(Localidad == '0001 HEROICA GUAYMAS')
#sumar datos por Semana epidemiologica
dfeeeguaymas <-dfeeeguaymas %>%
  group_by(Semana.Epidemiologica) %>% summarise(across(where(is.numeric), .fns = sum)) %>%
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
dfeeeguaymas

#Hermosillo
dfeeehermosillo<-dfeee %>% filter(Localidad == '0001 HERMOSILLO')
#sumar datos por Semana epidemiologica
dfeeehermosillo<-dfeeehermosillo %>%
  group_by(Semana.Epidemiologica) %>% summarise(across(where(is.numeric), .fns = sum)) %>%
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
dfeeehermosillo

#Navojoa
dfeeenavojoa<-dfeee %>% filter(Localidad == '0001 NAVOJOA')
#sumar datos por Semana epidemiologica
dfeeenavojoa<-dfeeenavojoa %>%
  group_by(Semana.Epidemiologica) %>% summarise(across(where(is.numeric), .fns = sum)) %>%
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
dfeeenavojoa


#Tipo de Estudio(Verificacion)
dfeev<-dfee %>% filter(Tipo.de.Estudio == 'Verificacion')
dfeev

#Filtrar por localidad 
#Obregon
dfeevobregon<-dfeev %>% filter(Localidad == '0001 CIUDAD OBREGÓN')
#sumar datos por Semana epidemiologica
dfeevobregon<-dfeevobregon %>%
  group_by(Semana.Epidemiologica) %>% summarise(across(where(is.numeric), .fns = sum)) %>%
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
dfeevobregon

#Guaymas
dfeevguaymas<-dfeev %>% filter(Localidad == '0001 HEROICA GUAYMAS')

#sumar datos por Semana epidemiologicadfeevguaymas %>% 
  
dfeevguaymas<-dfeevguaymas%>%
  group_by(Semana.Epidemiologica) %>% summarise(across(where(is.numeric), .fns = sum)) %>%
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
dfeevguaymas

#Hermosillo
dfeevhermosillo<-dfeev %>% filter(Localidad == '0001 HERMOSILLO')
#sumar datos por Semana epidemiologica
dfeevhermosillo<-dfeevhermosillo %>%
  group_by(Semana.Epidemiologica) %>% summarise(across(where(is.numeric), .fns = sum)) %>%
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
dfeevhermosillo

#Navojoa
dfeevnavojoa<-dfeev %>% filter(Localidad == '0001 NAVOJOA')
#sumar datos por Semana epidemiologica
dfeevnavojoa<-dfeevnavojoa %>%
  group_by(Semana.Epidemiologica) %>% summarise(across(where(is.numeric), .fns = sum)) %>%
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100), IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100), (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))
dfeevnavojoa


#"Num.Estudio"
