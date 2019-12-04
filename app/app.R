library(shiny)
library(shinythemes)
library(ggplot2)

#########     UI     ###########

ui <- fluidPage(h1("Kalkulator kredytowy"), theme = shinytheme("united"),
                
                sidebarLayout( 
                    
                    sidebarPanel(
                        h3("Parametry kredytu"),
                        br(), 
                        numericInput("oprocentowanie", "Oprocentowanie w %", value = 3, step = 0.1 ),
                        sliderInput("kapital", "Pożyczany kapitał", min = 0, max = 500000, value = 200000, step = 1000),
                        sliderInput("czas", "Liczba rat kredytu",   min = 0, max = 600, value = 240, step = 12)
                        # actionButton("update", "Przelicz")
                    ),
                    mainPanel( 
                        h1("Wynik"),
                        p("Orientacyjna wysokość raty i kwoty do spłaty"),   
                        
                        tabsetPanel(
                            
                            tabPanel( "Raty równe",
                                      br(),  
                                      strong ( textOutput( "rata") ) , #tekst wynikowy
                                      strong ( textOutput("total") ) , #tekst wynikowy
                                      h3("Wykaz rat kredytu"),
                                      dataTableOutput("df1")               ),
                            
                            tabPanel( "Raty malejące",
                                      br(), 
                                      strong (  textOutput("total2") ),
                                      h3("Charakterystyka rat kredytu"),
                                      verbatimTextOutput('summary'),
                                      h3("Wykaz rat kredytu"),
                                      dataTableOutput("df")                 ),
                            
                            tabPanel( "Porównanie",  
                                      h3("Wysokość raty w poszczególnych miesiącach"),
                                      plotOutput('plot')   )
                        )  #tabset
                    ) # main panel  
                ) #sidebarLayot
) #fluid



#########   SERVER   ###########
server <- function(input, output) {
    
    ## Wyliczenia do kredytu z ratą równą ##
    dane <- reactive({
        
        r <-  as.numeric(input$oprocentowanie * 0.01) # oprocentowanie
        K <-  as.numeric(input$kapital)               # kapitał początkowy
        n <-  as.numeric(input$czas)                  # okres kredytowania [miesiące]
        
        q <- 1+ r/12 
        rata <- K* q^n * (q-1)/(q^n-1) 
        total <- rata*n 
        
        raty_rowne <- data.frame( Nr_raty = 1:n, Rata=round(rata,digits = 2))
        
        rata  <-  format( rata ,  nsmall = 2, digits = 2,
                          big.mark = '  ', big.interval = 3,
                          decimal.mark = ',',
                          small.mark = '  ', small.interval = 3   )
        
        total <-  format(total,  nsmall = 2,
                         big.mark = '  ', big.interval = 3,
                         decimal.mark = ',',
                         small.mark = '  ', small.interval = 3   )
        
        list(rata, total, raty_rowne)
        
    })  
    #RATY MALEJĄCE
    
    dane2 <- reactive({
        
        r <-  as.numeric(input$oprocentowanie * 0.01) # oprocentowanie
        K <-  as.numeric(input$kapital)               # kapitał początkowy
        n <-  as.numeric(input$czas)                  # okres kredytowania [miesiące]
        
        R0 <- round( K/n , digits = 2)  #część kapitałowa
        R1 <- numeric(n)  #pusty wektor na część odsetkową rat od 1 do n
        R <-  numeric(n)   #pusty wektor na raty od 1 do n
        
        #wypełnienie wektorów
        for (i in 1:n)
        {  R1[i] =  round(   ( (K- (i-1)* R0)*r ) /12 , digits = 2)  #część odsetkowa i-tej raty
        R[i]  = R1[i]+R0  #całkowita wysokość i-tej raty
        next  }
        
        R_df <- data.frame(Nr_raty= 1:n, Rata= R, Odsetki= R1, Kapital = R-R1)
        
        list( R_df, R_min= min(R), R_mean= mean(R), R_max= max(R), F_total = sum(R)  )
        
    })
    
    ## Outputs ##
    
    #raty równe 
    output$rata    <- renderText( { paste0( "Rata kredytu wyniesie:   ",     dane()[[1]], " PLN" ) })
    output$total   <- renderText( { paste0( "Całkowita kwota do spłaty:  ",  dane()[[2]], " PLN" ) })
    output$df1     <- renderDataTable({       dane()[[3]]          })
    
    #raty malejące 
    output$df      <- renderDataTable({       dane2()[[1]]          })
    output$summary <- renderPrint({   summary ( dane2()[[1]] )       }) 
    
    output$total2  <- renderText( { 
        
        wynik <-  format( dane2()[[5]] , 
                          decimal.mark = ',', nsmall = 2,
                          big.mark = '  ',  big.interval = 3,
                          small.mark = '  ', small.interval = 3 )
        
        paste0( "Całkowita kwota do spłaty:   ",  wynik  ,     " PLN" )   })
    
    
    # podsumowanie i wykres
    output$plot <- renderPlot(  {
        
        rowne <- dane()[[3]]
        rowne$typ <- "Równe"
        malejace <- dane2()[[1]] 
        malejace$typ <- "Malejące"
        
        both <- bind_rows(rowne, malejace)
        
        ggplot(data=both)+
            geom_point(aes(x=Nr_raty, y=Rata, colour=typ))+
            ylab("Wysokość raty[PLN]") + 
            scale_x_continuous("Miesiąc", breaks = seq(0,nrow(rowne), by=12)  )         } )
    
}
# Run the application 
shinyApp(ui, server, options = list(display.mode = 'showcase'))
