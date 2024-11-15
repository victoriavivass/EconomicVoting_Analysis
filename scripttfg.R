##Script TFG Mar?a Victoria Vivas Guti?rrez

#Paquetes
library(ggplot2)
library(dplyr)
library(extrafont)
library(haven)
library(naniar) #para remplazar por NA
library(glmnet)
library(matrix)
library(nnet)
library(stargazer)
library(MASS)
library(rpart)
library(rpart.plot)
library(margins)
library(cowplot)
library(psych)
library(knitr)
library(kableExtra)
library(htmltools)


#Directorio de trabajo, el general 

setwd("C:/Users/Victoria/OneDrive/Escritorio/Ideas para el TFG")


#-----------------------------------------------------------------------------------------------------------------------
#Gr?ficos de la introducci?n

midata <- read.csv("datosCIS_2020-2024.csv", header = TRUE, sep = ";") %>%
  na.omit() %>%
  rename("malavaloraci?n" = "Valorecon_malaomuy.mala", 
         "principalproblema" = "Principal_Problema_Pa?.s_crisiseconómica",
         "segundoproblema" = "Segundo_Problema_crisiseconomica",
         "fecha" = "?..Fecha")

Sys.setlocale("LC_TIME", "Spanish")
midata$fecha <- as.Date(midata$fecha, format = "%d/%m/%Y")

# Crear etiquetas en formato "2021 - Enero"
midata$etiquetas_fechas <- format(midata$fecha, "%Y - %B")

#1. Primer grafico 

gr1 <- ggplot(midata, aes(x = fecha, y = malavaloración)) +
  geom_line() + geom_point(shape = 15, size = 1, color = "black") +
  scale_x_date(labels = function(x) format(x, "%Y - %B"),
    breaks = midata$fecha  
  ) +
  theme_classic(base_family = "Times New Roman") + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family="Times New Roman", size=8),  # Texto del eje X
    axis.text.y = element_text(family="Times New Roman", size= 10),  # Texto del eje Y
    axis.title.x = element_text(family="Times New Roman", size = 11),  # T?tulo del eje X
    axis.title.y = element_text(family="Times New Roman", size = 11),  # T?tulo del eje Y
    plot.title = element_text(hjust = 0.5, family="Times New Roman", size = 16),  # T?tulo del gr?fico
    plot.caption = element_text(hjust=0, family="Times New Roman", size=12)  # Caption del gr?fico
  )

gr1 <- gr1 + labs(
  x = "Fecha",
  y = "(%) Porcentaje de encuestados que valoran la situación económica de España 'Mal' o 'Muy mal'"
) 


#2. Segundo gr?fico 

gr2 <- ggplot(midata, aes(x = fecha, y = Porcentaje.total)) +
  geom_line() + geom_point(shape = 15, size = 1, color = "black") +
  scale_x_date(labels = function(x) format(x, "%Y - %B"),
               breaks = midata$fecha  
  ) +
  theme_classic(base_family = "Times New Roman") + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family="Times New Roman", size=8),  # Texto del eje X
    axis.text.y = element_text(family="Times New Roman", size= 10),  # Texto del eje Y
    axis.title.x = element_text(family="Times New Roman", size = 11),  # T?tulo del eje X
    axis.title.y = element_text(family="Times New Roman", size = 11),  # T?tulo del eje Y
    plot.title = element_text(hjust = 0.5, family="Times New Roman", size = 16),  # T?tulo del gr?fico
    plot.caption = element_text(hjust=0, family="Times New Roman", size=12)  # Caption del gr?fico
  ) + scale_fill_discrete (name = "Porcentaje Total", label = "Negro")

gr2 <- gr2 + geom_line(aes(x = fecha, y = principalproblema),  colour = "red") 
gr2 <- gr2 + geom_line(aes(x = fecha, y = segundoproblema), linetype = "dashed")


print(gr2)

gr2 <- gr2 + labs(
  x = "Fecha",
  y = "(%) Porcentaje de encuestados que responden que la económica es el primer o el segundo problema principal de Espa?a"
) 

#Guardar gr?ficos 

ggsave("valoraci?n1.png", plot = gr1, width = 10, height = 6, dpi = 300)
ggsave("principalproblema1.png", plot = gr2, width = 10, height = 6, dpi = 300)


#estadisticos descriptivos 

dir()
midata2 <- read.csv("datosCIS_2018_2019.csv", header = TRUE, sep = ";")
Sys.setlocale("LC_TIME", "Spanish")
midata2$fecha <- as.Date(midata2$Fecha, format = "%d/%m/%Y")

# Crear etiquetas en formato "2021 - Enero"
midata2$etiquetas_fechas <- format(midata2$Fecha, "%Y - %B")

midata2 <- midata2 %>% rename(Valoracion = Valorecon_malaomuy.mala)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
##Metodolog?a
#-------------------------------------------------------Elecciones del 2023---------------------------------------------------------------------------

setwd("C:/Users/Victoria/OneDrive/Escritorio/Ideas para el TFG/CIS Postelectoral 2023")
dir()
postelec_2023 <- read_sav("3420.sav")
midata2023 <- data.frame("Genero" = postelec_2023$SEXO, "Edad" = postelec_2023$EDAD, 
                   "Sociotropico" = postelec_2023$ECOESP, "Recuvoto" = postelec_2023$RECUVOTOG1R, 
                     "Educacion" = postelec_2023$ESTUDIOS, "Ingresos" = postelec_2023$INGRESHOG, 
                   "Ideologia" = postelec_2023$ESCIDEOL)
                  
                    


#Limpieza de datos 

midata2023 <- na.omit(midata2023) #partimos de 8986 observaciones 

#Paso 1: cargarme los NC NS y los partidos que no quiero...etc de las variables seleccionadas 


midata2023 <- midata2023 %>%
  mutate(Sociotropico = ifelse(Sociotropico %in% c(8, 9), NA_real_, Sociotropico),
    Recuvoto = ifelse(Recuvoto %in% c(1,2,3,21), Recuvoto, NA_real_),
    Ingresos = ifelse(Ingresos %in% c(8, 9), NA_real_, Ingresos),
    Ideologia = ifelse(Ideologia %in% c(98,99), NA_real_, Ideologia)
  )


#Paso 2: pasar casi todo a factores y a?adir etiquetas

str(midata2023) #todos son numericas menos genero y edad que aun no las he tocado y salen como character

midata2023 <- midata2023 %>% 
  mutate(
    Genero = factor(Genero, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    Edad = as.numeric(Edad),
    Ideologia = as.numeric(Ideologia),
    Sociotropico = factor(Sociotropico, levels = c(1, 2, 3, 4, 5), 
                          labels = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala")),
    Recuvoto = factor(Recuvoto, levels = c(1, 2, 3, 21), 
                      labels = c("PSOE", "PP", "VOX", "Sumar")),
    Ingresos = factor(Ingresos, levels = c(1, 2, 3, 4, 5, 6), 
                      labels = c("M?s de 5.000 Euros", "De 3.901 a 5.000 Euros", "De 2.701 a 3.900 Euros", 
                                 "De 1.801 a 2.700 Euros", "De 1.100 a 1.800 Euros", "Menos de 1.100 Euros")),
    Ingresos = relevel(Ingresos, ref = "Menos de 1.100 Euros")
  )


                               
midata2023 <- midata2023 %>%
  mutate(
    Educacion = factor(case_when(
      Educacion %in% c(1, 2) ~ "Educación Básica o Ninguna",
      Educacion %in% c(3, 4, 5) ~ "Educación Secundaria y Formación Profesional(FP)",
      Educacion == 6 ~ "Educaci?n Superior",
      TRUE ~ as.character(Educacion)  # por si acaso hay valores inesperados
    ), levels = c("Educaci?n B?sica o Ninguna", "Educación Secundaria y Formación Profesional(FP)", 
                  "Educaci?n Superior"))
  )

midata2023 <- midata2023 %>%
  mutate(
    Edad = cut(Edad,
               breaks = c(18, 30, 40, 50, 60, Inf),  # Definir puntos de corte
               labels = c("De 18 a 30 años", "De 31 a 40 años", "De 41 a 50 años", 
                          "De 51 a 60 años", "M?s de 60 años"),  # Etiquetas correspondientes
               include.lowest = TRUE)  # Incluir el valor m?s bajo en el primer intervalo
  )




#Paso 3: recodificar variable independiente (X1) en hipotesis

midata2023$voto <- ifelse(midata2023$Recuvoto == "PSOE" | midata2023$Recuvoto == "Sumar", 1, 
                          ifelse(midata2023$Recuvoto == "PP" | midata2023$Recuvoto == "VOX", 0, NA_real_))

# Convertir 'voto' a un factor con niveles y etiquetas descriptivas
midata2023$voto <- factor(midata2023$voto, levels = c(0, 1), labels = c("No apoyo a la coalición gobernante", "Apoyo a la coalición gobernante"))

                     
#Paso 4: Eliminar NAs

naeliminado2023 <- colSums(is.na(midata2023)) #de qu? observaciones 

midata2023 <- na.omit(midata2023)

#-------------------------
#Modelo 1 - 2023

# Ajustar modelo multinomial h1

m2023 <- polr(Sociotropico ~ . - Recuvoto, data = midata2023, Hess = TRUE)
m2023sim <- polr(Sociotropico ~ voto, midata2023, Hess = TRUE)

# Mostrar resumen del modelo
summary(m2023)

#Modelo 2 - 2023, logit de la segunda hip?tesis 

logit2023 <- glm(voto ~ . -Recuvoto, family = "binomial", midata2023)
logit2023sim <- glm(voto ~ Sociotropico, family = "binomial", midata2023)

#-------------------------------------------------------Elecciones del 2019 NOVIEMBRE -----------------------------------------------------------------------------

setwd("C:/Users/Victoria/OneDrive/Escritorio/Ideas para el TFG/CIS Postelectoral 2019 noviembre")
postelec_2019dic <- read_sav("3269.sav")

midata2019d <- data.frame("Genero" = postelec_2019dic$C9, "Edad" = postelec_2019dic$C10 ,
                          "Sociotropico" = postelec_2019dic$A1, "Recuvoto" = postelec_2019dic$B22, 
                          "Educacion" = postelec_2019dic$ESTUDIOS, "Ingresos" = postelec_2019dic$C20, 
                          "Ideologia" = postelec_2019dic$C3)
                           
# Limpieza de datos
midata2019d <- midata2019d %>%
  mutate(
    Ideologia = ifelse(Ideologia %in% c(98, 99), NA_real_, Ideologia),
    Sociotropico = ifelse(Sociotropico %in% c(8, 9), NA_real_, Sociotropico),
    Recuvoto = ifelse(Recuvoto %in% c(1, 2, 4, 18, 21, 50, 6, 67), Recuvoto, NA_real_),
    Ingresos = ifelse(Ingresos %in% c(99, 1), NA_real_, Ingresos),
    Sociotropico = factor(Sociotropico, levels = c(1, 2, 3, 4, 5), 
                          labels = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"))
  ) %>%
  mutate(
    Educacion = factor(case_when(
      Educacion %in% c(1, 2) ~ "Educación Básica o Ninguna",
      Educacion %in% c(3, 4, 5) ~ "Educación Secundaria y Formación Profesional(FP)",
      Educacion == 6 ~ "Educación Superior",
      TRUE ~ NA_character_
    ), levels = c("Educación Básica o Ninguna", "Educación Secundaria y Formación Profesional(FP)", 
                  "Educación Superior"))
  ) %>%
  mutate(
    Edad = cut(Edad,
               breaks = c(18, 30, 40, 50, 60, Inf),  
               labels = c("De 18 a 30 años", "De 31 a 40 años", "De 41 a 50 años", 
                          "De 51 a 60 años", "Más de 60 años"),  
               include.lowest = TRUE)  
  ) %>%
  mutate(
    Genero = factor(Genero, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    Recuvoto = factor(Recuvoto, levels = c(1, 2, 4, 18, 21, 6, 67), 
                      labels = c("PP", "PSOE", "C'S", "VOX", "Unidas Podemos",
                                 "En Com? Podem", "En Com?n-Unidas Podemos"))
  ) %>%
  mutate(
    Ingresos = case_when(
      Ingresos %in% c(10, 11)    ~ "Menos de 1.100 Euros",   
      Ingresos == 9              ~ "De 1.100 a 1.800 Euros",
      Ingresos %in% c(7, 8)      ~ "De 1.801 a 2.700 Euros",
      Ingresos == 6              ~ "De 2.701 a 3.900 Euros",
      Ingresos %in% c(2, 3, 4, 5) ~ "De 3901 a 5.000 Euros",
      TRUE                       ~ "M?s de 5.000 Euros"  
    ),
    Ingresos = factor(Ingresos, 
                      levels = c("M?s de 5.000 Euros", "De 3901 a 5.000 Euros", 
                                 "De 2.701 a 3.900 Euros", "De 1.801 a 2.700 Euros",
                                 "De 1.100 a 1.800 Euros", "Menos de 1.100 Euros")),
    Ingresos = relevel(Ingresos, ref = "Menos de 1.100 Euros")
  ) %>%
  mutate(
    voto = case_when(
      Recuvoto %in% c("PSOE", "Unidas Podemos", "En Común Podem", "En Común-Unidas Podemos") ~ 1,
      Recuvoto %in% c("PP", "VOX", "C'S") ~ 0,
      TRUE ~ NA_real_  
    ),
    voto = factor(voto, levels = c(0, 1), labels = c("No apoyo a la coalición gobernante", "Apoyo a la coalición gobernante"))
  )



#Paso 5: Eliminar NAs

naeliminadonov2019 <- colSums(is.na(midata2019d))
midata2019d <- na.omit(midata2019d)

m2019nov <- polr(Sociotropico ~ .-Recuvoto, midata2019d, Hess = TRUE)
m2019novsim <- lm(Sociotropico ~ voto, midata2019d, Hess = TRUE)

#Modelo 2 - 2023, logit de la segunda hip?tesis 

logit2019noviembre <- glm(voto ~ . -Recuvoto, family = "binomial", midata2019d)
logit2019noviembresimple <- glm(voto ~ Sociotropico, family = "binomial", midata2019d)

#-------------------------- Elecciones del 2019 Abril ------------------
#Nota: has cogido el barómetro de junio porque no habia voto egotrópico en el postelectoral 

setwd("C:/Users/Victoria/OneDrive/Escritorio/Ideas para el TFG/CIS Postelectoral 2019 abril")
postelec_2019abril <- read_sav("3248.sav")

midata2019a <- data.frame("Genero" = postelec_2019abril$P41, "Edad" = postelec_2019abril$P42, 
                          "Sociotropico" = postelec_2019abril$P6 , "Recuvoto" = postelec_2019abril$P39A, 
                          "Educacion" = postelec_2019abril$P47A, "Ingresos" = postelec_2019abril$P54, 
                          "Ideologia" = postelec_2019abril$P32)
                           
midata2019a <- na.omit(midata2019a) #habia 1024 NA al importar la base de datos 

#Limpieza de datos 


#Paso 1: cargarme los NC NS y los partidos que no quiero...etc de las variables seleccionadas 

midata2019a <- midata2019a %>%
  mutate(Sociotropico = ifelse(Sociotropico %in% c(8,9), NA_real_, Sociotropico),
         Recuvoto = ifelse(Recuvoto %in% c(1,2,3,4,5, 6, 18), Recuvoto, NA_real_),
         Ingresos = ifelse(Ingresos %in% c(99,99), NA_real_, Ingresos), 
         Ideologia = ifelse(Ideologia %in% c(98,99), NA_real_, Ideologia))
        

midata2019a <- midata2019a %>%
  mutate(
    Educacion = factor(case_when(
      Educacion %in% c(1, 2, 3, 4, 5, 8, 7) ~ "Educación Básica o Ninguna",
      Educacion %in% c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22) ~ "Educación Secundaria y Formaci?n Profesional(FP)",
      Educacion %in% c (23, 24, 25, 26) ~ "Educación Superior",
      TRUE ~ NA_character_  # Manejar otros casos como NA
    ), levels = c("Educación Básica o Ninguna", "Educación Secundaria y Formación Profesional(FP)", 
                  "Educación Superior"))
  )

midata2019a <- midata2019a %>%
  mutate(
    Edad = cut(Edad,
               breaks = c(18, 30, 40, 50, 60, Inf),  # Definir puntos de corte
               labels = c("De 18 a 30 años", "De 31 a 40 años", "De 41 a 50 años", 
                          "De 51 a 60 años", "M?s de 60 años"),  # Etiquetas correspondientes
               include.lowest = TRUE)  # Incluir el valor m?s bajo en el primer intervalo
  )


#Paso 2: pasar casi todo a factores y a?adir etiquetas

str(midata2019a) #todos son numericas menos genero y edad que aun no las he tocado y salen como character

midata2019a <- midata2019a %>% 
  mutate(Genero = factor(Genero, levels = c(1,2), labels = c("Hombre", "Mujer")),
         Sociotropico = factor(Sociotropico, levels = c(2,3,4,5), labels = c("Buena", "Regular", "Mala", "Muy mala")),
         Recuvoto = factor(Recuvoto, levels = c(1,2,3,4,5, 6, 18), labels = c("PP", "PSOE", "Podemos", "Ciudadanos", "IU",
                                                                              "En Com? Podem", "VOX")))

                                                                              
midata2019a <- midata2019a %>%
  mutate(Ingresos = as.numeric(Ingresos),  # Aseg?rate de que Ingresos es num?rico
         Ingresos = case_when(
           Ingresos %in% c(1, 2, 3, 4)     ~ "Menos de 1.100 Euros",
           Ingresos %in% c(5, 6)           ~ "De 1.100 a 1.800 Euros",
           Ingresos == 7                   ~ "De 1.801 a 2.700 Euros",
           Ingresos %in% c(8, 9)           ~ "De 2.701 a 3.900 Euros",
           Ingresos == 10                  ~ "De 3.901 a 5.000 Euros",
           Ingresos == 11                  ~ "M?s de 5.000 Euros",
           Ingresos %in% c(98, 99)         ~ NA_character_  # Maneja N.S. y N.C. como NA
         )) %>%
  mutate(Ingresos = factor(Ingresos, 
                           levels = c("Menos de 1.100 Euros", "De 1.100 a 1.800 Euros", 
                                      "De 1.801 a 2.700 Euros", "De 2.701 a 3.900 Euros", 
                                      "De 3.901 a 5.000 Euros", "M?s de 5.000 Euros")),
         Ingresos = relevel(Ingresos, ref = "Menos de 1.100 Euros"))

                                                                              

#Paso 3: recodificar variable independiente (X1)

midata2019a <- midata2019a %>%
  mutate(voto = case_when(
    Recuvoto %in% c("PSOE", "Podemos","En Común Podem", "IU") ~ 1,
    Recuvoto %in% c("PP", "VOX", "Ciudadanos") ~ 0,
    TRUE ~ NA_real_  # Asegura que todos los dem?s casos sean NA
  ))

# Ahora convierte 'voto' en un factor con las etiquetas apropiadas
midata2019a$voto <- factor(midata2019a$voto, levels = c(0, 1), labels = c("No apoyo a la coalición gobernante", "Apoyo a la coalici?n gobernante"))


#Paso 5: Eliminar NAs

naeliminadosabril <- colSums(is.na(midata2019a))
midata2019a <- na.omit(midata2019a)


m2019abril <- polr(Sociotropico ~ . -Recuvoto, midata2019a, Hess = TRUE)
m2019abrilsim <- polr(Sociotropico ~ voto, midata2019a)

stargazer(m2019abrilsim, m2019abril, m2019novsim, m2019nov, m2023sim, m2023, type = "html", out = "model_comparison.html")

#Modelo 2 - abril, logit de la segunda hip?tesis 

midata2019a$Sociotropico <- factor(midata2019a$Sociotropico)

logit2019abril <- glm(voto ~ . -Recuvoto, family = "binomial", midata2019a)
logit2019abrilsimple <- glm(voto ~ Sociotropico, family = "binomial", midata2019a)


stargazer(logit2019abrilsimple, logit2019abril, logit2019noviembresimple, logit2019noviembre, logit2023sim, logit2023, type = "html", out = "model_comparisonlogit.html")

####--------------------------------------------------------------------------------------------------------------------------------
#GR?FICOS DE EFECTOS MARGINALES

midata2019a$Sociotropico <- as.numeric(midata2019a$Sociotropico)
midata2019d$Sociotropico <- as.numeric(midata2019d$Sociotropico)
midata2023$Sociotropico <- as.numeric(midata2023$Sociotropico)


logit2019amarg <- glm(voto ~ Sociotropico, family = "binomial", midata2019a)
logit2019dmarg <- glm(voto ~ Sociotropico, family = "binomial", midata2019d)
logit2023marg <- glm(voto ~ Sociotropico, family = "binomial", midata2023)

# Calcular los efectos marginales para cada modelo
margins_2019abrilsim <- margins(logit2019amarg)
margins_2019noviembresim <- margins(logit2019dmarg)
margins_2023sim <- margins(logit2023marg)

# Convertir los efectos marginales a data frames
margins_2019abrilsim_df <- as.data.frame(margins_2019abrilsim)
margins_2019noviembresim_df <- as.data.frame(margins_2019noviembresim)
margins_2023sim_df <- as.data.frame(margins_2023sim)

# Calcular la desviaci?n est?ndar de los efectos marginales
margins_2019abrilsim_df$se.dydx_Sociotropico <- sqrt(margins_2019abrilsim_df$Var_dydx_Sociotropico)
margins_2019noviembresim_df$se.dydx_Sociotropico <- sqrt(margins_2019noviembresim_df$Var_dydx_Sociotropico)
margins_2023sim_df$se.dydx_Sociotropico <- sqrt(margins_2023sim_df$Var_dydx_Sociotropico)

# Ajustar la escala del eje Y de -0.5 a 0.5
ylim_min <- -0.3
ylim_max <- 0.3

# Gr?fico de Sociotropico para Abril 2019
plot_2019abrilsim <- ggplot(margins_2019abrilsim_df, aes(x = factor(Sociotropico, levels = c(1, 2, 3, 4, 5), labels = c("Muy buen estado de la econom?a", "Buen estado de la econom?a", "Regular estado de la econom?a", "Mal estado de la econom?a", "Muy mal estado de la econom?a")), y = dydx_Sociotropico)) +
  geom_point() +
  geom_errorbar(aes(ymin = dydx_Sociotropico - 1.96 * se.dydx_Sociotropico, ymax = dydx_Sociotropico + 1.96 * se.dydx_Sociotropico), width = 0.2) +
  ylim(ylim_min, ylim_max) +
  labs(title = "Elecciones de abril de 2019", x = NULL, y = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))

# Gr?fico de Sociotropico para Noviembre 2019
plot_2019noviembresim <- ggplot(margins_2019noviembresim_df, aes(x = factor(Sociotropico, levels = c(1, 2, 3, 4, 5), labels = c("Muy buen estado de la econom?a", "Buen estado de la econom?a", "Regular estado de la econom?a", "Mal estado de la econom?a", "Muy mal estado de la econom?a")), y = dydx_Sociotropico)) +
  geom_point() +
  geom_errorbar(aes(ymin = dydx_Sociotropico - 1.96 * se.dydx_Sociotropico, ymax = dydx_Sociotropico + 1.96 * se.dydx_Sociotropico), width = 0.2) +
  ylim(ylim_min, ylim_max) +
  labs(title = "Elecciones de noviembre de 2019", x = NULL, y = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))

# Gr?fico de Sociotropico para 2023
plot_2023sim <- ggplot(margins_2023sim_df, aes(x = factor(Sociotropico, levels = c(1, 2, 3, 4, 5), labels = c("Muy buen estado de la econom?a", "Buen estado de la econom?a", "Regular estado de la econom?a", "Mal estado de la econom?a", "Muy mal estado de la econom?a")), y = dydx_Sociotropico)) +
  geom_point() +
  geom_errorbar(aes(ymin = dydx_Sociotropico - 1.96 * se.dydx_Sociotropico, ymax = dydx_Sociotropico + 1.96 * se.dydx_Sociotropico), width = 0.2) +
  ylim(ylim_min, ylim_max) +
  labs(title = "Elecciones de julio de 2023", x = NULL, y = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))

# Combinar los gr?ficos en uno solo para Sociotropico
combined_plot_sociotropico <- plot_grid(plot_2019abrilsim, plot_2019noviembresim, plot_2023sim, nrow = 3)

# Mostrar el gr?fico combinado
print(combined_plot_sociotropico)

# Guardar los gr?ficos individualmente
ggsave("plot_2019abrilsim.png", plot = plot_2019abrilsim, width = 8, height = 6)
ggsave("plot_2019noviembresim.png", plot = plot_2019noviembresim, width = 8, height = 6)
ggsave("plot_2023sim.png", plot = plot_2023sim, width = 8, height = 6)

# Guardar el gr?fico combinado
ggsave("combined_plot_sociotropico.png", plot = combined_plot_sociotropico, width = 18, height = 22)

#ejemplo de cómo saqué los descriptivos 

descriptives3 <- dfSummary(midata2023); print(descriptives3, method = "pander", file = "descriptives_table2.html"
