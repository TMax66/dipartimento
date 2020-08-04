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
        filter(Matrici==input$pr) %>% 
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
        filter(Matrici==input$pr) %>%
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
  
  
  
  
  
  ####grafici andamento n.esami/settimana x reparto e per i topclient####
  
  
  # topClient<-reactive {
  #   dati %>% 
  #   filter(reparto=="input$rep") %>% 
  #   group_by(destfatt) %>% 
  #   summarise(Esami=sum(esami)) %>% 
  #   arrange(desc(Esami)) %>% 
  #   top_n(input$top)
  # }
  
  
  

  
    # dati %>% 
    #   filter(destfatt %in% topClient()$destfatt) %>% 
    #   group_by(destfatt, settimana) %>% 
    #   summarise(Esami=sum(esami)) %>% 
    #   ggplot(aes(x=settimana, y=Esami, col=destfatt))+geom_point()+geom_line()+
    #   theme_light(base_size = 16)+labs(y="n.esami/settimana")+
    #   theme(strip.text.x = element_text(size = 18))+
    #   theme(legend.position = element_blank())
    
    x<- reactive({ 
      dati %>% 
      filter(reparto == input$rep) %>% 
      group_by(destfatt, settimana) %>% 
      summarise(Esami=sum(esami))
      })
    
    z<-reactive({dati %>% 
      filter(reparto == input$rep) %>% 
      group_by(destfatt, settimana) %>% 
      summarise(Esami=sum(esami)) %>% 
      group_by(destfatt) %>% 
      summarise(n=n()) })
    
   
    
#Grafico clienti####
   output$timecl <-renderPlot(
     
     if(input$rep!="Tutti")
     {   
     
     x() %>% 
       left_join(z(), by="destfatt") %>% 
      # filter(n > input$nset) %>% 
       ggplot(aes(x=settimana, y=Esami, col=destfatt))+geom_line()+
       theme(legend.position = "none")+
       gghighlight(mean(Esami) >=input$mes, label_key = destfatt) 
     } else
       
     {    
       
      
       x1<- dati %>%
           group_by(destfatt, settimana) %>% 
           summarise(Esami=sum(esami))
      
       z1<-dati %>% 
         group_by(destfatt, settimana) %>% 
         summarise(Esami=sum(esami)) %>% 
         group_by(destfatt) %>% 
         summarise(n=n())
       
     x1%>% 
       left_join(z1, by="destfatt") %>% 
      # filter(n > input$nset) %>% 
       ggplot(aes(x=settimana, y=Esami, col=destfatt))+geom_line()+
       theme(legend.position = "none")+
       gghighlight(mean(Esami) >= input$mes, label_key = destfatt) 
       
     }
       
       
   )
      
#Tabella clienti####  
   output$clienti <- DT::renderDataTable(server = FALSE,
   class = 'cell-border stripe', rownames=FALSE,
   extensions = 'Buttons',options = list(dom="Brtip", pageLength = 10,
   searching = FALSE,paging = TRUE,autoWidth = TRUE,
   buttons = c('excel')),
     
     if(input$rep!="Tutti")
       
     {
     
     
     x()%>% 
       left_join(z(), by="destfatt") %>% 
       filter(Esami > input$mes) %>% 
       group_by("Cliente" = destfatt) %>% 
         summarise("Totale esami" = sum(Esami), 
                   "Media settimanale" = round(mean((Esami)),2),
                   "N.settimane" = max(n)) %>% 
       arrange(desc("Totale esami"))
     
     }else
       
     {
       x1<- dati %>%
         group_by(destfatt, settimana) %>% 
         summarise(Esami=sum(esami))
       
       z1<-dati %>% 
         group_by(destfatt, settimana) %>% 
         summarise(Esami=sum(esami)) %>% 
         group_by(destfatt) %>% 
         summarise(n=n())
       x1%>% 
         left_join(z1, by="destfatt") %>% 
         group_by("Cliente" = destfatt, n) %>% 
         summarise("Totale esami" = sum(Esami), 
                   "Media settimanale" = round(mean((Esami)),2),
                   "N.settimane" = max(n)) %>% 
         select(-n) %>% 
         arrange(desc("Totale esami")) 
        

     })
   
   
#Pivot Table####

output$pivot <- renderRpivotTable({
  dx<-dati %>% 
    dplyr::select(regione, provincia, reparto, Matrici, "Clienti"=destfatt, esami)
  rpivotTable(dx,aggregatorName="Integer Sum", vals="esami", 
              onRefresh = htmlwidgets::JS(
                        "function(config) {
                        Shiny.onInputChange('pivot', document.getElementById('pivot').innerHTML); 
                        }"))
})


pivot_tbl <- eventReactive(input$pivot, {
     tryCatch({
       input$pivot %>%
         read_html %>%
         html_table(fill = TRUE) %>%
         .[[2]]
     }, error = function(e) {
       return()
     })
   })

observe({
  if (is.data.frame(pivot_tbl()) && nrow(pivot_tbl()) > 0) {
    shinyjs::enable("download_pivot")
  } else {
    shinyjs::disable("download_pivot")
  }
})

output$download_pivot <- downloadHandler(
  filename = function() {
    "pivot.xlsx"
  },
  content = function(file) {
    writexl::write_xlsx(pivot_tbl(), path = file)
  }
  
)
          
   
   
   
   
   
   
   
   
   
   
   
   
   
  
  output$tab <- DT::renderDataTable(
                dati[, 1:15], 
                filter = 'top',class = 'cell-border stripe',
                options = list(dom = 'Bfrtip',searching = TRUE,
                               pageLength = 10))
  

}


