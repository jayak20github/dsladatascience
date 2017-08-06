library(shiny)

numChoices <- c(1000, 2000, 3000)
colChoices <- c('positivity','anger','anticipation','disgust','fear','joy',
                'sadness','surprise','trust')

shinyUI(
  navbarPage("Twitter Analysis",
             tabPanel("Load Tweets",
                      fluidPage(
                        sidebarLayout(
                          # Sidebar with a slider and selection inputs
                          sidebarPanel(
                            # Text box
                            textInput("searchString",
                                      "Search Twitter for:",
                                      "brexit"),
                            selectInput("numTweets", "Number of Tweets:",
                                        choices = numChoices),
                            checkboxInput("rt_remove", "Eliminate Retweets",
                                          value=T),
                            #checkboxInput("isUser", "Search is a Screen Name",
                            #              value=F),
                            actionButton("update", "Search")
                          ),
                          mainPanel(plotOutput("plot"),
                                    verbatimTextOutput("tweetCount")
                          )
                        )
                      )),
             tabPanel("Sentiments",
                      fluidPage(
                        titlePanel("Sentiment Analysis"),
                        mainPanel(plotOutput("sentiment"))
                      ))

          
  )
)
