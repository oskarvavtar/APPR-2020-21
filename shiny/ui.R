

shinyUI(
  fluidPage(
    theme = shinytheme("superhero"),
    navbarPage("",
               tabPanel("Starosti izvajalcev",
                        titlePanel(title=h2("Starosti izvajalcev v letu nastopa", 
                                            align="center")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "Desetletje",
                                        label = "Desetletje:",
                                        choices =  c("1970-a"="1970", 
                                                     "1980-a"="1980", 
                                                     "1990-a"="1990", 
                                                     "2000-a"="2000",
                                                     "2010-a"="2010"),
                                        selected = "1990")),
                          
                          mainPanel(
                            plotOutput(outputId = "starosti"))
                        )),
               tabPanel("Pogostost zvrsti",
                        titlePanel(title=h2("Pogostost zvrsti na festivalih ter lestvicah",
                                            align="center")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "Zvrst",
                                        label = "Zvrst:",
                                        choices = c("Rock",
                                                    "Pop",
                                                    "Indie",
                                                    "Folk",
                                                    "Hiphop",
                                                    "Jazz",
                                                    "Soul",
                                                    "Punk",
                                                    "EDM",
                                                    "Metal"),
                                        selected = "Soul")),
                          mainPanel(
                            plotOutput(outputId="pogostost_zanrov"))
                          ))

    ) 
  ) 
) 
