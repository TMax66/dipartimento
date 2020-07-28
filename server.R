server<-function(input, output) { 
  
  
  output$es<-renderTable(
    
    if (input$pr=="Tutte")
    {   
      dati %>%  
        group_by(reparto) %>% 
        summarise(tot=round(sum(esami, na.rm=TRUE),0)) %>% 
        arrange(desc(tot)) %>% 
        mutate(n.esami=as.character(tot)) %>% 
        select(reparto, n.esami)
      
    } else
      
    {
      dati %>% 
        filter(gruppoM==input$pr) %>% 
        group_by(reparto) %>% 
        summarise(tot=round(sum(esami, na.rm=TRUE),0)) %>% 
        arrange(desc(tot)) %>% 
        mutate(n.esami=as.character(tot)) %>% 
        select(reparto, n.esami)
    }
  )
  
  output$es2<-renderTable(
    
    if (input$pr=="Tutte")
    {
      dati %>%
        group_by(Autocontrollo) %>%
        summarise(tot=round(sum(esami, na.rm=TRUE),0)) %>%
        arrange(desc(tot)) %>%
        mutate(n.esami=as.character(tot)) %>%
        select(Autocontrollo, n.esami)
      
      
    } else
      
    {
      dati %>%
        filter(gruppoM==input$pr) %>%
        group_by(Autocontrollo) %>%
        summarise(tot=round(sum(esami, na.rm=TRUE),0)) %>%
        arrange(desc(tot)) %>%
        mutate(n.esami=as.character(tot)) %>%
        select(Autocontrollo, n.esami)
    }
  )
  
  output$time <- renderPlot(
    
    if(input$tt=="Refertazione")
    {
      dati %>%
        ggplot(aes(x=TR))+geom_bar()+xlim(0,10)+
        theme_light(base_size = 16)+facet_wrap(~reparto)+xlab("Tempi di refertazione (giorni): data emissione Rdp - data accettazione")+theme(strip.text.x = element_text(size = 18))+ylab("n.esami")
    }else
    {
      dati %>%
        ggplot(aes(x=TG))+geom_bar()+xlim(0,5)+
        theme_light(base_size = 16)+facet_wrap(~reparto)+xlab("Tempi di gestione del campione (giorni): data inizio analisi - data accettazione")+theme(strip.text.x = element_text(size = 18))+ylab("n.esami")
    }
  )
  
  output$time2 <- renderPlot(
      
     dati %>%
      group_by(reparto, settimana) %>%
      summarise(tot=sum(esami, na.rm=TRUE)) %>%
      ggplot(aes(x=settimana, y=tot))+geom_point()+geom_line()+
      facet_wrap(~reparto)+theme_light(base_size = 16)+labs(y="n.esami/settimana")+
      theme(strip.text.x = element_text(size = 18))
  )
  
  
  output$clienti <- renderPlot(
    
    dati %>% 
      mutate(Matrici=fct_lump_min(gruppoM, 603, other_level = "Altro")) %>% 
      mutate(Clienti=fct_lump_min(destfatt, 261, other_level = "Altri")) %>% 
      filter(Clienti=input$cl) %>% 
      group_by(reparto,Matrici) %>% 
      summarise(Esami=sum(esami)) %>% 
      arrange(desc(Esami)) %>% 
      ggplot()+
      aes(x=Matrici,y=Esami)+
      geom_bar(stat="identity")+
      coord_flip()+
      facet_wrap(~reparto)

    
  )
  
  
  
  
  output$tab <- DT::renderDataTable(
                dati[, 1:15], 
                filter = 'top',class = 'cell-border stripe',
                options = list(dom = 'Bfrtip',searching = TRUE,
                               pageLength = 10))
  

  }