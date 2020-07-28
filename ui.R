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
                             c("Tutte", "3.CARNE, PRODOTTI A BASE DI CARNE, CACCIAGIONE E POLLAME",
                               "22.IGIENE DELLA MACELLAZIONE", "23.ACQUA", "24.TAMPONI AMBIENTALI",
                               "1.PRODOTTI LATTIERO CASEARI"))
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

           tabPanel("DATI", 
                   
                      fluidRow(
                        DT::dataTableOutput("tab")
                      )
                    )
           )
                    
             

