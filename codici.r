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
library(rpivotTable)
  

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
  
  group_by(gruppoM) %>% 
  summarise(Esami=sum(esami)) %>%  
  arrange(desc(Esami))


dati %>%  
  group_by(destfatt) %>% 
  summarise(Esami=sum(esami)) %>% 
  arrange(desc(Esami))

  
dati %>% mutate(Clienti=fct_lump_min(destfatt, 261, other_level = "Altri")) %>% 
   group_by(Clienti) %>% 
   summarise(Esami=sum(esami)) %>% 
   arrange(desc(Esami))






dati %>% 
  mutate(Matrici=fct_lump_min(gruppoM, 603, other_level = "Altro")) %>% 
  mutate(Clienti=fct_lump_min(destfatt, 261, other_level = "Altri")) %>% 
  group_by(reparto,Matrici, Clienti) %>% 
  summarise(Esami=sum(esami)) %>% 
  arrange(desc(Esami)) %>% 
  ggplot()+
  aes(x=Matrici,y=Esami)+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~Clienti)
  
  


