
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
  output$ustanovitve <- renderPlot({
    izvajalci_ostalo %>%
      filter(Leto%/%10*10 == input$Desetletje_2) %>%
      group_by(Ustanovitev) %>%
      summarise(Število=n()) %>%
      
      ggplot(aes(x=Ustanovitev, y=Število)) +
      geom_col()
    
  })
})
