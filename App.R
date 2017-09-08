#' Portfolio Analysis through sentiment of facebook and twitter.
#' @export
sentapp <- function(){
  pkgs <-c('quantmod','Rfacebook','stringr','twitteR','ROAuth','httr','plyr', 'ggplot2', 'plotly','forecast','tseries', 'DT','shiny')
  for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
  for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

  pkgs2 <- c('gtrendsR')
  for(p in pkgs2) if(p %in% rownames(installed.packages()) == FALSE) {devtools::install_github("PMassicotte/gtrendsR")}
  for(p in pkgs2) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    # Application title
    titlePanel("Sentiment Analysis Portfolio App"),

    # Sidebar with fields for adding stock to portfolio
    sidebarLayout(
      sidebarPanel(
        textInput('ticker', label="Enter a Ticker", placeholder = "Stock Ticker (ex. AAPL - Apple inc.)"),
        textInput('Facebook_token', label="Facebook Token", placeholder="Facebook Token needed to fetch data"),
        dateRangeInput("dates", label = ("Date Range (yy/mm/dd)"), format = "yy/mm/dd", start="2017/01/01"),
        actionButton(inputId = "Add", label = "Add Stock"),
        actionButton(inputId = "Clear", label = "Clear Portfolio")
      ),
      # Show portfolio
      mainPanel(
        DT::dataTableOutput("portfolio_table")
      )
    )
  )

  server <- function(input, output) {
    values <- reactiveValues()
    quote_SPY <- getQuote('SPY', what=yahooQF("Last Trade (Price Only)"))[,2]

    values$portfolio <- data.frame(Ticker = NA, Name = NA, Price = NA, twitter_sentiment = NA, facebook_sentiment = NA, Correlation = NA)

    # API Credentials
    api_key <- ""
    api_secret <- ""
    token <- ""
    token_secret <- ""

    # Connect to Twitter
    setup_twitter_oauth(api_key, api_secret, token, token_secret)

    # POSITIVE/NEGATIVE WORD DICTIONARY - HU AND LIU OPINION LEXICON
    positive = scan('./rawdata/positive-words.txt', what='character', comment.char=';')
    negative = scan('./rawdata/negative-words.txt', what='character', comment.char=';')

    # ABILITY TO EXPAND EXISTING DICTIONARY
    bad_text = c(negative, 'wtf', 'epicfail')
    good_text = c(positive, 'upgrade', ':)')

    # PERFORMS SENTIMENT ANALYSIS ON TWITTER FEED WHEN CALLED
    score.sentiment = function(sentences, good_text, bad_text, .progress='none') {
      # Get the score of every tweet
      scores = laply(sentences, function(sentence, good_text, bad_text) {

        # clean up sentences with R's regex-driven global substitute, gsub():. Acquired from the Reference above
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        #Remove emojis
        sentence <- gsub("[^0-9A-Za-z///' ]", "", sentence)
        sentence = tolower(sentence)

        # use str_split to split the sentence by words.
        word.list = str_split(sentence, "\\s+")
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)

        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, good_text)
        neg.matches = match(words, bad_text)

        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)

        # Get final score of the tweet by taking score of positive words - negative words
        score = sum(pos.matches) - sum(neg.matches)

        return(score)
      }, good_text, bad_text, .progress=.progress )

      scores.df = data.frame(score=scores, text=sentences)
      return(scores.df)
    }

    # ADDING STOCKS TO PORTFOLIO
    newEntry <- observeEvent(input$Add, {
      withProgress(message = "Getting Stock Data", value = 0, {
        n <- 30
        # Begin
        incProgress(1/n, detail = paste("WORKING"))

        # Get data from input fields
        ticker <- toupper(input$ticker)
        ticker <- as.character(ticker)
        quote <- getQuote(ticker, what=yahooQF("Name"))[,2]
        quote_price <- getQuote(input$ticker, what=yahooQF("Last Trade (Price Only)"))[,2]
        company_name <- str_extract(quote, "\\w+")

        twitter_sentiment <- 0
        facebook_sentiment <- 0
        stockCor <- 0

        # FACEBOOK API CREDENTIALS
        token2 <- input$Facebook_token

        if (input$Facebook_token == "") {
          showModal(modalDialog(
            tags$div(
              tags$b("You must provide a Facebook 2 hour access token in order for us to process your request.")
            ),
            tags$br(),
            tags$div(
              tags$b(tags$a(href="https://developers.facebook.com/tools/explorer/", "Facebook Token Retrievable Here"))
            ),
            easyClose = TRUE,
            size = "l"
          ))
        }
        else {
          # Get Facebook Data for Sentiment Analysis
          incProgress(5/n, detail = paste("Fetching Facebook Data"))
          facebook_page <- getPage(company_name, token2, n = 50, since = input$dates[1], until = input$dates[2] )
          no_error = TRUE

          if (is.data.frame(facebook_page) & length(facebook_page) == 0) {
            no_error = FALSE
          }
          if (no_error) {

            post <- getPost(facebook_page$id[1], token2, n.comments = 50, likes = FALSE)

            comments <- post$comments

          }

          # Get public tweets about this company
          incProgress(10/n, detail = paste("Fetching Twitter Data"))
          tweets = searchTwitter(company_name, n=500)

          # format the tweets to what we can use
          feed = lapply(tweets, function(t) t$getText())

          # Get twitter sentiment
          incProgress(15/n, detail = paste("Analyzing Twitter Sentiment"))
          analyze_twitter = score.sentiment(feed, good_text, bad_text, .progress="text")

          # Get Facebook sentiment
          incProgress(20/n, detail = paste("Analyzing Facebook Sentiment"))
          if (no_error) {
            analyze_facebook = score.sentiment(comments$message, good_text, bad_text, .progress="text")
          }

          # Sum the individual scores for overall sentiment
          sentiment_twitter = sum(analyze_twitter$score)
          if (no_error) {
            sentiment_facebook = sum(analyze_facebook$score)
          } else {
            sentiment_facebook = 0
          }

          # If sentiment above 0, then smiley. Less than 0, frowny. Otherwise, neutral
          if (sentiment_twitter < 0) { twitter_sentiment = ':('}
          else if (sentiment_twitter > 0) { twitter_sentiment = ':)' }
          else { twitter_sentiment = ':|'}

          if (sentiment_facebook < 0) { facebook_sentiment = ':('}
          else if (sentiment_facebook > 0) { facebook_sentiment = ':)' }
          else { facebook_sentiment = 0}

          # Getting Google Trends Data
          incProgress(25/n, detail = paste("Analyzing Google Trends"))
          symbols <- getSymbols(Symbols = c(input$ticker),auto.assign = FALSE)
          trends = gtrends(company_name,geo = "US","today+5-y")
          date = trends$interest_over_time$date
          hits = trends$interest_over_time$hits
          ds <- data.frame(Date = date, symbols[0:length(hits)], hits) ##User input here

          ##correlation
          stockCor <- cor(ds[,5], ds$hits)

          #Finished adding
          incProgress(30/n, detail = paste("Finished"))
        }
      })

      if (twitter_sentiment == 0) {
        twitter_sentiment <- "Not Found"
      }
      if (facebook_sentiment == 0) {
        facebook_sentiment <- "Not Found"
      }

      newLine <- c(ticker, company_name, quote_price, twitter_sentiment, facebook_sentiment, round(stockCor, digits=3))
      values$portfolio <- rbind(values$portfolio, newLine)
    })

    # CLEARING STOCKS FROM PORTFOLIO
    clearEntries <- observeEvent(input$Clear, {
      values$portfolio <- data.frame(Ticker = NA, Name = NA, Price = NA, twitter_sentiment = NA, facebook_sentiment = NA, Correlation = NA)
    })

    # RENDERING DATA OF INDIVIDUAL ADDED STOCK WHEN CLICKED
    observeEvent(input$portfolio_table_cell_clicked, {
      name <- input$portfolio_table_cell_clicked
      # FACEBOOK API CREDENTIALS
      token2 <- input$Facebook_token

      if (is.null(name$value) || name$col != 1) {
        return ()
      }
      else {
        if (input$Facebook_token == "") {
          showModal(modalDialog(
            tags$div(
              tags$b("You must provide a Facebook 2 hour access token in order for us to process your request.")
            ),
            tags$br(),
            tags$div(
              tags$b(tags$a(href="https://developers.facebook.com/tools/explorer/", "Facebook Token Retrievable Here"))
            ),
            easyClose = TRUE,
            size = "l"
          ))
        }
        else {
          withProgress(message="Making Plot", value=0, {
            n <- 20

            # Begin getting stock data
            incProgress(1/n, detail = paste("Fetching Stock"))
            quote <- getQuote(name$value, what=yahooQF("Name"))[,2]
            companyname <- str_extract(quote,"\\w+")

            # Get facbook information
            incProgress(5/n, detail = paste("Rendering Facebook Data"))
            page <- getPage(companyname, token2, n = 50, since = input$dates[1], until = input$dates[2] )

            if (is.data.frame(page) & length(page) == 0) {
              most_likes <- "Not Found"
              most_shares <- "Not Found"
              most_comments <- "Not Found"
              most_liked_comment <- "Not Found"
            } else {
              most_likes <- page[which.max(page$likes_count),]$message
              most_shares <- page[which.max(page$shares_count),]$message
              most_comments <- page[which.max(page$comments_count),]$message
              post <- getPost(page$id[1], token2, n.comments = 50, likes = FALSE)
              comments <- post$comments
              most_liked_comment <- comments[which.max(comments$likes_count),]$message
            }

            # Google Trends Information
            incProgress(10/n, detail = paste("Rendering Google Trends Data"))
            trends = gtrends(companyname,geo = "US","today+5-y")
            date = trends$interest_over_time$date
            hits = trends$interest_over_time$hits

            most_likes = gsub("[^0-9A-Za-z///' ]", "", most_likes)
            most_shares = gsub("[^0-9A-Za-z///' ]", "", most_shares)
            most_comments = gsub("[^0-9A-Za-z///' ]", "", most_comments)
            most_liked_comment = gsub("[^0-9A-Za-z///' ]", "", most_liked_comment)

            # Get Stock Name
            symbol <- getSymbols(Symbols = c(name$value), auto.assign = FALSE)

            # Modal
            incProgress(15/n, detail = paste("Rendering Google Trends Data"))
            showModal(modalDialog(
              tags$div(
                tags$b("Most Liked Post: "), most_likes,
                tags$br(),
                tags$b("Most Shared Post: "),
                most_shares,
                tags$br(),
                tags$b("Most Shared Comments: "),
                most_comments,
                tags$br(),
                tags$b("Most Liked Comments: "),
                most_liked_comment
              ),
              tags$div(
                renderPlot({
                  adf.test(hits, alternative = "stationary")

                  fit<-auto.arima(hits, seasonal=TRUE)

                  tsdisplay(residuals(fit), lag.max=50, main='(1,1,1) Model Residuals')

                  fcast <- forecast(fit, h=20)
                  plot(fcast)
                })
              ),
              tags$div(
                renderPlotly({
                  ds <- data.frame(Date = date, symbol[0:length(hits)], hits) ##User input here

                  ##correlation
                  stockCor <- cor(ds[,5], ds$hits) ##User Input here
                  if(stockCor < 0.05){
                    clr = "red"
                  }else{
                    clr = "purple"
                  }
                  plot_ly(ds, x = ~Date) %>%
                    add_lines(y = ~ds[,5], name = quote, color = I(clr)) %>%
                    add_lines(y = ~hits, name = "Trends", color = I("forestgreen")) %>%
                    layout(
                      title = "Stock Prices vs Trends",
                      xaxis = list(
                        rangeselector = list(
                          buttons = list(
                            list(
                              count = 3,
                              label = "3 mo",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 mo",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 yr",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "YTD",
                              step = "year",
                              stepmode = "todate"),
                            list(step = "all"))),

                        rangeslider = list(type = "date")),

                      yaxis = list(title = "Trends"))
                })
              ),
              easyClose = TRUE,
              size = "l"
            ))
          })
        }
      }
    })

    # RENDERING PORTFOLIO OF STOCKS
    output$portfolio_table <- DT::renderDataTable(values$portfolio[-1,], selection = 'single', class="cell-border strip hover")
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}
