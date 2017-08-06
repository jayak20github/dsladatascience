library(shiny)

function(input, output, session) {
  
  statuses <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Gathering tweets...")
        getTweets(input$searchString, input$numTweets, 
                  input$rt_remove)
      })
    })
  })
  
  textdata <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    getTextData(statuses())
  })
  
  sentiments <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    #isolate({
    withProgress({
      setProgress(message = "Gathering sentiments...")
      sentiments <- getSentiments(textdata())
    })
    #})
  })
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    wordcloud_rep(textdata(), scale=c(4,0.5),
                  min.freq=3, max.words=100,
                  colors=brewer.pal(8, "RdBu"), random.order=F, 
                  rot.per=0.1, use.r.layout=F)
  })
  
  output$tweetCount  <- renderText({
    df <- statuses()
    paste("Number of Tweets Found: ", as.character(nrow(df)))
  })
  
  output$sentiment <- renderPlot({
    v <- sentiments()
    emotions <- data.frame("count"=colSums(v[,c(1:8)]))
    emotions <- cbind("sentiment" = rownames(emotions), emotions)
    ggplot(data = emotions, aes(x = sentiment, y = count)) +
      geom_bar(aes(fill = sentiment), stat = "identity") +
      xlab("Sentiment") + ylab("Total Count") + 
      scale_fill_brewer(palette='RdBu') + 
      theme_bw() + theme(legend.position='none')
  })
  

}