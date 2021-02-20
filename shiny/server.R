
shinyServer(function(input, output) {
  output$starosti <- renderPlot({
    izvajalci_ostalo %>%
      filter(Leto%/%10*10 == input$Desetletje) %>%
      group_by(Starost) %>%
      summarise(Število=n()) %>%
      
    ggplot(aes(x=Starost, y=Število)) +
      geom_col()
  
  })
  output$pogostost_zanrov <- renderPlot({
    podatki <- zanri %>% 
      select(Desetletje, Vrste, input$Zvrst) %>%
      rename(Pogostost=3)
    
    ggplot(data=podatki, aes(x = Desetletje, y = Pogostost, col=Vrste)) + 
      geom_point() + 
      geom_line()
  })
})
