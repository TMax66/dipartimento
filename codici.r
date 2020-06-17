###DEPLOY

library(rsconnect)
rsconnect::setAccountInfo(name='izslerapp',
                          token='10A9891BDD1F8D2341EDC222AE8D386F',
                          secret='NWl1tMriCmkV2G9EXXv0z3SnTYftNynuRZwuvDcs')
rsconnect::deployApp("dash.Rmd", 
                     appName = "dash", 
                     account = "izslerapp") 














library(tidyverse)
library(readxl)

dati<- read_excel("dip19nonuff.xlsx")
dati<-dati %>% 
  mutate(Autocontrollo=ifelse(reparto=="Sede Territoriale di Bergamo"|
                                reparto=="Sede Territoriale di Pavia"|
                                reparto=="Sede Territoriale di Binago"|
                                reparto=="Sede Territoriale di Lodi"|
                                reparto=="Sede Territoriale di Milano", "Sede Territoriale di Milano",
                              ifelse(reparto=="Sede Territoriale di Brescia"|
                                       reparto=="Sede Territoriale di Cremona"|
                                       reparto=="Sede Territoriale di Mantova", "Alimenti Brescia", 
                                     "Sede Territoriale di Sondrio"
                              )
  )) 


  
dati %>% 
  filter()
  group_by(reparto) %>% 
  summarise(tot=sum(esami, na.rm=TRUE)) %>% 
  arrange(desc(tot)) %>% 
  top_n(30) %>% 
  View()


dati %>% 
  mutate(TR=as.numeric(difftime(datrdp, dati$datareg, units = "days")),
         TG=as.numeric(difftime(datainizio, dataconf, units = "days")))%>% 
  ggplot(aes(x=TR))+geom_bar()+xlim(0,10)+
  theme_light()+facet_wrap(~reparto)










  dati %>% 
    group_by(reparto,destfatt) %>% 
    summarise(n=n(),
      tot=sum(esami, na.rm=TRUE)) %>% 
    mutate(i=n/tot) %>% 
    arrange(i)%>% 
    top_n(30) %>% 
    View()


  #####################################################


  ms<-dati %>% 
    group_by(reparto, settimana) %>% 
    summarise(tot=sum(esami, na.rm=TRUE)) %>% 
    group_by(reparto) %>% 
    summarise(x=mean(tot)) %>% 
    select(x)
  
 dati %>% 
    group_by(reparto, settimana) %>% 
    summarise(tot=sum(esami, na.rm=TRUE)) %>% 
    ggplot(aes(x=settimana, y=tot, group=1))+geom_line()+geom_point()+
    facet_wrap(~reparto)+ theme_light()
    

    
