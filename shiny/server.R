#library(shiny)

shinyServer(function(input, output) {
  output$starosti <- renderPlot({
    vektor <- rep(1, length(izvajalci_ostalo$Starost))
    podatki <- izvajalci_ostalo %>% 
      mutate(Desetletje=(Leto%/%10*10)) 
    podatki$Pojavitev <- vektor 
    podatki <- podatki %>%
      filter(Desetletje == input$Desetletje) %>%
      select(Starost, Pojavitev) %>%
      group_by(Starost) %>%
      summarise(Število=sum(Pojavitev))
    
    ggplot(data=podatki, aes(x=Starost, y=Število)) +
      geom_col()
  
  })
  output$pogostost_zanrov <- renderPlot({
    podatki2 <- zanri  
    prikazani <- as.vector(c(podatki2$Desetletje, podatki2$Vrste, podatki2$input))
    podatki2 <- df[,prikazani]
    
    ggplot(data=podatki2, aes(x = Desetletje, y = Pogostost, col=Vrste)) + 
      geom_point() + 
      geom_line()
  })
})
