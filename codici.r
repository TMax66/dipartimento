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
library(gghighlight)
  

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
                     'ZUPPE-BRODI-SALSE' = "6.ZUPPE, BRODI, SALSE",
                     CEREALI = "7.CEREALI E PRODOTTI DELLA PANETTERIA",
                     'PIATTI-PREPARATI' = "17.PIATTI PREPARATI",
                     ACQUA = "23.ACQUA",
                     'TAMPONI-AMBIENTALI'= "24.TAMPONI AMBIENTALI",
                     'ALTRE MATRICI'= "25.MATRICI VARIE",
                     'IGIENE DELLA MAC' = "22.IGIENE DELLA MACELLAZIONE",
                     'ALTRI ALIMENTI' = "21.ALTRI ALIMENTI",
                     'ERBE-SPEZIE-ECC' = "9.ERBE, SPEZIE, CAFFE', TE'",
                     'UOVA E DERIVATI' = "2.UOVA E OVOPRODOTTI",
                     'PESCI-CROST-MOLL' = "4.PESCI, CROSTACEI, MOLLUSCHI",
                     ADDITIVI = "19.ADDITIVI",
                     'PROD.LATTIERO CASEARI' = "1.PRODOTTI LATTIERO CASEARI",
                     'VERDURE-FRUTTA' = "8.FRUTTA E VERDURE",
                     'FRUTTA SECCA' = "16.FRUTTA SECCA A GUSCIO RIGIDO, SPUNTINI",
                     DOLCIUMI = "15.DOLCIUMI",
                     GELATI = "13.GELATI E DESSERT",
                     'PROD ALIM PARTICOLARE' = "18.PRODOTTI DESTINATI AD UNA ALIMENTAZIONE PARTICOLARE",
                     BEVANDE= "10.BEVANDE NON ALCOLICHE",
                     'GRASSI-OLI'= "5.GRASSI ED OLI")
  
  )
  



dati %>% 
  
  group_by(gruppoM) %>% 
  summarise(Esami=sum(esami)) %>%  
  arrange(desc(Esami))


x<-dati %>%  
  group_by(destfatt) %>% 
  summarise(Esami = sum(esami)) %>% 
  arrange(Esami)  

  

x %>% 
  arrange(desc(Esami)) %>% 
  #top_n(5) %>% 
  ggplot(aes(destfatt,Esami))+
  ggQC::stat_pareto(point.color = "red",
              point.size = 3,
              line.color = "black",
              bars.fill = c("blue", "orange")) 



x %>% 
  top_frac(0.15) %>% 
  arrange(desc(Esami))

  
dati %>% mutate(Clienti=fct_lump_min(destfatt, 261, other_level = "Altri")) %>% 
   group_by(Clienti) %>% 
   summarise(Esami=sum(esami),
             N=) %>% 
   arrange(desc(Esami))


topClient<-dati %>% 
  # filter(reparto=="Sede Territoriale di Bergamo") %>% 
  group_by(destfatt) %>% 
  summarise(Esami=sum(esami)) %>% 
  arrange(desc(Esami)) %>% 
top_n(5)


dati %>% 
  filter(destfatt %in% topClient$destfatt) %>% 
  group_by(destfatt, settimana) %>% 
  summarise(Esami=sum(esami)) %>% 
  ggplot(aes(x=settimana, y=Esami, col=destfatt))+geom_point()+geom_line()+
  theme_light(base_size = 16)+labs(y="n.esami/settimana")+
  theme(strip.text.x = element_text(size = 18))+
  theme(legend.position = "none")



x<-  
    dati  %>%
    filter(destfatt %in% topClient$destfatt) %>% 
    group_by(destfatt, settimana) %>%
    summarise(Esami=sum(esami))

  z<-dati %>%
    filter(destfatt %in% topClient$destfatt) %>% 
    group_by(destfatt, settimana) %>%
    summarise(Esami=sum(esami)) %>%
    group_by(destfatt) %>%
    summarise(n=n()) 

  x%>% 
    left_join(z, by="destfatt") %>% 
    group_by("Cliente" = destfatt, n) %>% 
    summarise("Totale esami" = sum(Esami), 
              "Media settimanale" = round(mean((Esami)),2),
              "N.settimane" = max(n)) %>% 
    select(-n) %>% 
    arrange(desc("Totale esami")) 
  
