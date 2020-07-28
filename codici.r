###DEPLOY

# library(rsconnect)
# rsconnect::setAccountInfo(name='izslerapp',
#                           token='10A9891BDD1F8D2341EDC222AE8D386F',
#                           secret='NWl1tMriCmkV2G9EXXv0z3SnTYftNynuRZwuvDcs')
# rsconnect::deployApp("dash.Rmd", 
#                      appName = "dash", 
#                      account = "izslerapp") 
#######################################################################à
library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinythemes)
library(DT)
library(tm)
  

dati2<- read_excel("autocontrollo.xlsx")
dati<-dati2 %>% 
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
  



dati %>%
  filter(prova==input$pr) %>%
  group_by(Autocontrollo) %>%
  summarise(tot=round(sum(esami, na.rm=TRUE),0)) %>%
  arrange(desc(tot)) %>%
  mutate(n.esami=as.character(tot)) %>%
  select(Autocontrollo, n.esami)

dati %>%
  group_by(Autocontrollo) %>%
  summarise(tot=round(sum(esami, na.rm=TRUE),0)) %>%
  arrange(desc(tot)) %>%
  mutate(n.esami=as.character(tot)) %>%
  select(Autocontrollo, n.esami)

