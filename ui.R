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
                             
                             c("Tutte", unique(as.character(dati$Matrici))))
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
                        sliderInput("mes", "top clienti", 
                                    min=1, max= 50, value=10)),
                   
                    mainPanel(
                      DT::dataTableOutput("clienti"),
                      hr(),
                      br()
                      #plotOutput("timecl")
                      )
                    
                    )), 
           tabPanel("TABELLA PIVOT", 
                      fluidPage(
                        fluidRow(
                          downloadButton("download_pivot", label = "Excel")), 
                          fluidRow(
                          column(6,div(style="height:10px"),rpivotTableOutput("pivot")
                                ))
                          
                        ))
           ,
         
                  
                    

           tabPanel("DATI", 
                   
                      fluidRow(
                        DT::dataTableOutput("tab")
                      )
                    )
           )

                    
             

