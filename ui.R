ui<-navbarPage("Dipartimento Area Territoriale Lombardia: Attività Non Ufficiale (2019)",
    theme = shinytheme("cerulean"),
           
    tabPanel("ESAMI",
             sidebarLayout(
               sidebarPanel(
                 
                 tags$p("Criteri di estrazione da Business Object:"),

tags$p("Anno di registrazione:2019"), 

tags$p("Reparti che eseguono le prove: Sezioni Territoriali della Lombardia"),

tags$p("Tipologia campioni: Non ufficiali"),

tags$p("Settore: Alimenti Uomo"), 

                
                hr(),
                
                 selectInput("pr", "Seleziona Gruppo Matrice", 
                             c("Tutte", "CARNI E DERIVATI",
                               "IGIENE DELLA MAC", "ACQUA", "TAMPONI-AMBIENTALI",
                               "PROD.LATTIERO CASEARI"))
               ), 
               mainPanel(
                 column(6, 
                        tableOutput("es")), 
                 
                 column(6, 
                        tags$p("Ipotesi di ridistribuzione esami su Milano, Brescia e Sondrio"),
                        tableOutput("es2"))
                 
               )
             )),
           
           
           tabPanel("TEMPI",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("tt", "Tempi", 
                                    c("Refertazione", "Gestione"))), 
                      
                    mainPanel(
                       plotOutput("time")
                    )
                      )
                    ),
           
           tabPanel("ATTIVITA'", 
                    
                    fluidRow(
                      plotOutput("time2")
                    )

                    ), 
           tabPanel("CLIENTI", 
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("rep", "Seleziona Reparto", 
                            c( "Tutti", unique(as.character(dati$reparto))) ),
                        sliderInput("mes", "media settimanale n. di esami", 
                                    min=1, max= 30, value=10),
                        sliderInput("nset", "Numero di settimane di conferimenti",
                                    min=1, max=20, value = 10)),
                      
                   
                    mainPanel(
                     
                      plotOutput("timecl"), 
                      hr(),
                      br(),
                      DT::dataTableOutput("clienti")
                        
                      )
                    
                    )), 
           tabPanel("TABELLA PIVOT", 
                      fluidPage(
                        fluidRow(
                          column(6,div(style="height:10px"),rpivotTableOutput("pivot") )
                          
                        ))
           ),
         
                  
                    

           tabPanel("DATI", 
                   
                      fluidRow(
                        DT::dataTableOutput("tab")
                      )
                    )
           )

                    
             

