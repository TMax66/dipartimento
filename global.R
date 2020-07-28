library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinythemes)
library(DT)


dati<- read_excel("autocontrollo.xlsx")
dati<-dati %>% 
  mutate(Autocontrollo=ifelse(
                                reparto=="Sede Territoriale di Pavia"|
                                reparto=="Sede Territoriale di Binago"|
                                reparto=="Sede Territoriale di Lodi"|
                                reparto=="Sede Territoriale di Milano", "Sede Territoriale di Milano",
                              ifelse(reparto=="Sede Territoriale di Bergamo"|
                                reparto=="Sede Territoriale di Brescia"|
                                       reparto=="Sede Territoriale di Cremona"|
                                       reparto=="Sede Territoriale di Mantova", "Alimenti Brescia", 
                                     "Sede Territoriale di Sondrio"
                              )
  ), TR=difftime(datrdp, dataconf, units = "days"),
         TG=difftime(datainizio, dataconf, units = "days"), 
 settimana=week(dataconf))
