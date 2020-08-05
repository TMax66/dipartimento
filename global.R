library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinythemes)
library(DT)
library(gghighlight)
library(rpivotTable)
library(shinyjs)
library(writexl)
library(rvest)


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
 settimana=week(dataconf), 
 
 Sezioni=fct_recode(reparto, PV="Sede Territoriale di Pavia",
                    VA="Sede Territoriale di Binago",
                    LO= "Sede Territoriale di Lodi",
                    MI= "Sede Territoriale di Milano",
                    BG= "Sede Territoriale di Bergamo",
                    CR= "Sede Territoriale di Cremona",
                    MN= "Sede Territoriale di Mantova",
                    SO= "Sede Territoriale di Sondrio", 
                    BS= "Sede Territoriale di Brescia"),
 
 Matrici=fct_recode(gruppoM, 
                    'CARNI E DERIVATI' = "3.CARNE, PRODOTTI A BASE DI CARNE, CACCIAGIONE E POLLAME",
                    'ALTRE MATRICI' = "6.ZUPPE, BRODI, SALSE",
                    'ALTRE MATRICI' = "7.CEREALI E PRODOTTI DELLA PANETTERIA",
                    'ALTRE MATRICI' = "17.PIATTI PREPARATI",
                    ACQUA = "23.ACQUA",
                    'TAMPONI-AMBIENTALI'= "24.TAMPONI AMBIENTALI",
                    'ALTRE MATRICI'= "25.MATRICI VARIE",
                    'IGIENE DELLA MAC' = "22.IGIENE DELLA MACELLAZIONE",
                    'ALTRE MATRICI' = "21.ALTRI ALIMENTI",
                    'ALTRE MATRICI' = "9.ERBE, SPEZIE, CAFFE', TE'",
                    'ALTRE MATRICI' = "2.UOVA E OVOPRODOTTI",
                    'ALTRE MATRICI' = "4.PESCI, CROSTACEI, MOLLUSCHI",
                    'ALTRE MATRICI' = "19.ADDITIVI",
                    'PROD.LATTIERO CASEARI' = "1.PRODOTTI LATTIERO CASEARI",
                    'ALTRE MATRICI' = "8.FRUTTA E VERDURE",
                    'ALTRE MATRICI' = "16.FRUTTA SECCA A GUSCIO RIGIDO, SPUNTINI",
                    'ALTRE MATRICI' = "15.DOLCIUMI",
                    'ALTRE MATRICI' = "13.GELATI E DESSERT",
                    'ALTRE MATRICI' = "18.PRODOTTI DESTINATI AD UNA ALIMENTAZIONE PARTICOLARE",
                    'ALTRE MATRICI'= "10.BEVANDE NON ALCOLICHE",
                    'ALTRE MATRICI'= "5.GRASSI ED OLI")

 )


# Matrici=fct_recode(gruppoM, 
#                    'CARNI E DERIVATI' = "3.CARNE, PRODOTTI A BASE DI CARNE, CACCIAGIONE E POLLAME",
#                    'ZUPPE-BRODI-SALSE' = "6.ZUPPE, BRODI, SALSE",
#                    CEREALI = "7.CEREALI E PRODOTTI DELLA PANETTERIA",
#                    'PIATTI-PREPARATI' = "17.PIATTI PREPARATI",
#                    ACQUA = "23.ACQUA",
#                    'TAMPONI-AMBIENTALI'= "24.TAMPONI AMBIENTALI",
#                    'ALTRE MATRICI'= "25.MATRICI VARIE",
#                    'IGIENE DELLA MAC' = "22.IGIENE DELLA MACELLAZIONE",
#                    'ALTRI ALIMENTI' = "21.ALTRI ALIMENTI",
#                    'ERBE-SPEZIE-ECC' = "9.ERBE, SPEZIE, CAFFE', TE'",
#                    'UOVA E DERIVATI' = "2.UOVA E OVOPRODOTTI",
#                    'PESCI-CROST-MOLL' = "4.PESCI, CROSTACEI, MOLLUSCHI",
#                    ADDITIVI = "19.ADDITIVI",
#                    'PROD.LATTIERO CASEARI' = "1.PRODOTTI LATTIERO CASEARI",
#                    'VERDURE-FRUTTA' = "8.FRUTTA E VERDURE",
#                    'FRUTTA SECCA' = "16.FRUTTA SECCA A GUSCIO RIGIDO, SPUNTINI",
#                    DOLCIUMI = "15.DOLCIUMI",
#                    GELATI = "13.GELATI E DESSERT",
#                    'PROD ALIM PARTICOLARE' = "18.PRODOTTI DESTINATI AD UNA ALIMENTAZIONE PARTICOLARE",
#                    BEVANDE= "10.BEVANDE NON ALCOLICHE",
#                    'GRASSI-OLI'= "5.GRASSI ED OLI")
# 
# )




