#Original Credit Goes to Tim Wilson of Analytics Demystified. Tim makes great snippet notes 
#and gives credit where it is due as well.  I added some stuff, modified to make shiny app
#deployment easier as a webapp.  Inspiration links as well as resources are below.  Really..
#All you need to do is register an API key with GA API via web app oAuth 2.0.  If you don't
#know how to do that- then you should probably ask a developer for help.  Sorry, I'm
#not a fan of typing notes for others, but try to show where I learned what I did along the way!

# App to generate a word cloud of site search terms. The user has to authenticate
# and choose a view. This is the Shiny-fication of some code created by Sébastien 
# Brodeur, which he demo'd at Superweek Hungary 2017. See his original post/code at:
# https://www.linkedin.com/pulse/using-r-get-real-insights-from-your-searched-terms-words-brodeur
#
library(shiny)              # We must web-enable this whole thing
library(DT)                 # For the frequency table display
library(shinyjs)


# There's a wrinkle for running this on shinyapps.io that requires using the Github
# version of googleAuthR. I don't know if this is actually still an issue or not, 
# actually. See: https://twitter.com/ryanpraski/status/783754506681155584
library(googleAuthR)        # To prompt for authentication by the user
library(googleAnalyticsR)   # For the pulling of the data
library(tidyverse)          # For data transformations -- primarily just uses dplyr commands

# The libraries needed for working with the text.
library(tm)
library(SnowballC)
library(wordcloud)

# This is important for your API connection to happen with shiny & Google Analytics API webapp
#You need to create an account via developers console with GA Reporting API and Create Credentials
#With Webapp and use your localhost address in redirect URl, and/or shinyapp url for redirect to deploy
#app successfully on shiny.  Your bread and butter is right here buddy: http://code.markedmondson.me/googleAuthR/
# and here: https://lesliemyint.wordpress.com/2017/01/01/creating-a-shiny-app-with-google-login/
# and... here: http://code.markedmondson.me/googleAuthR/index.html#google-api-setup
# and lastly here... http://code.markedmondson.me/googleAuthR/articles/google-authentication-types.html#authentication-within-shiny

#This is just what I added more notes to ellaborate with below on line 43-44
# options("googleAuthR.webapp.client_id" = "[GOOGLE APP CLIENT ID]")
# options("googleAuthR.webapp.client_secret" = "[GOOGLE APP CLIENT SECRET]")


options("googleAuthR.webapp.client_id" = "Enter your client ID here from the API setup Credentials you got")
options("googleAuthR.webapp.client_secret" = "Enter Client Secret here- again this is from the Google API auth setup")

# All we need to do is read the GA data, so we can limit the scope pretty
# severely.
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/analytics",
                                          "https://www.googleapis.com/auth/analytics.readonly"))


####################
# Set up the different options for interaction
####################

# DATE OPTIONS
# This could also be set as date selectors easily enough, but, for now, it's just set
# as some preset options. As a note, even though the values are being set as numerics 
# here, they actually get treated as characters, so they have to be converted 
# back to numerics when setting start_date in the get_base_data() function.
daterange_options <- list("Last 7 Days" = 7,
                          "Last 30 Days" = 30,
                          "Last 60 Days" = 60,
                          "Last 90 Days" = 90)

################
# Define server logic
################

server <- function(input, output) {
  
  # Get the view ID (user-selected). I'd be lying if I said I fully understood this
  # piece -- pretty much lifted it straight from Mark Edmondson's example at:
  # http://code.markedmondson.me/googleAnalyticsR/shiny.html. Except... used the
  # JS option: https://mark.shinyapps.io/googleAuthRMarkdown/
  access_token <- callModule(gar_auth_js, "auth_module")
  
  # Get the accounts list
  ga_account <- reactive({
    validate(
      need(access_token(), "Authenticate")
    )
    with_shiny(google_analytics_account_list, shiny_access_token = access_token())
  })
  
  view_id <- callModule(authDropdown, "auth_menu", ga.table = ga_account)
  
  # Reactive function to actually pull the data. This will get run if
  # the view is changed or if the date range is changed.
  get_base_data <- reactive({
    
    # Calculate the start and end dates.
    start_date <- as.character(Sys.Date()-as.numeric(input$daterange)-1)
    end_date <- as.character(Sys.Date()-1)
    
    # Pull the data. Note this limits the results to 10000 rows. You
    # can probably fiddle around with that if need be -- possible just
    # set max = -1 and see if it borks on you.
    ga_data <- with_shiny(google_analytics_4,
                          viewId = view_id(),
                          date_range = c(start_date,end_date),
                          metrics = "searchUniques",
                          dimensions = "searchKeyword",
                          order = order_type("searchUniques", "DESCENDING", "VALUE"),
                          anti_sample = TRUE,
                          max = 10000,
                          shiny_access_token = access_token())
  })
  
  ############################
  # Create a term document matrix for the data. 
  create_term_doc_matrix <- reactive({
    
    # Get the data
    wrdcld_data <- get_base_data()
    
    #Convert UTF-8 to ASCII 
    wrdcld_data$searchKeyword <- iconv(wrdcld_data$searchKeyword, "UTF-8", "ASCII") 
    
    # Repeat keyword by number of searches
    #  "A", 3
    #  "B", 2
    #  "C", 1
    # Becomes:
    #  "A"
    #  "A"
    #  "A"
    #  "B"
    #  "B"
    #  "C"
    
    
    wrdcld_data <- data.frame(searchKeyword = rep(wrdcld_data$searchKeyword, 
                                                  wrdcld_data$searchUniques))
    
    # 10,000 rows at a time to keep from running into memory limitation issues.
    if(nrow(wrdcld_data) > 10000){
      wrdcld_data_sample <- na.omit(as.data.frame(wrdcld_data$'searchKeyword'[sample(1:nrow(wrdcld_data), 10000)]))
      colnames(wrdcld_data_sample) <- "searchKeyword"
    } else {
      wrdcld_data_sample <- wrdcld_data
    }
    
    # Create a corpus 
    wrdcld_data_corpus <- Corpus(DataframeSource(data.frame(as.character(wrdcld_data_sample$'searchKeyword'))))
    
    # Cleaning, Cleaning, Cleaning that dataaaaaa:
    wrdcld_data_corpus <- tm_map(wrdcld_data_corpus, content_transformer(tolower))
    
    # Remove stopwords: a, the, as, etc. add whatever else you think you need
    wrdcld_data_corpus <- tm_map(wrdcld_data_corpus, function(x) removeWords(x, stopwords("english")))
    
    # Stem words: comptes + compte = compt
    wrdcld_data_corpus <- tm_map(wrdcld_data_corpus, stemDocument, language = "english") 
    
    # Remove any punctuation
    wrdcld_data_corpus <- tm_map(wrdcld_data_corpus, removePunctuation)
    
    # Create a Term Document Matrix
    wrdcld_data_tdm <- TermDocumentMatrix(wrdcld_data_corpus)
    
  })
  
  ############################
  # Do all of the manipulation to get to a frequency table that can be
  # used to both generate the word cloud and to generate the frequency table.
  create_freq_table <- reactive({
    
    # Get the full term document matrix
    term_doc_matrix <- create_term_doc_matrix()
    
    # Create the actual frequency table
    wrdcld_data_m <- as.matrix(term_doc_matrix)
    wrdcld_data_v <- sort(rowSums(wrdcld_data_m), decreasing=TRUE)
    wrdcld_data_d <- data.frame(word = names(wrdcld_data_v), freq=wrdcld_data_v)
    
    # Remove any of the exclusion terms that are entered.
    if(!is.null(input$exclude_terms)){
      # Take the comma-delimited list of terms and split them out to be a
      # character vector. The ", ?" regEx is so that this will work with
      # or without a space following the comma
      remove_terms <- unlist(strsplit(input$exclude_terms,", ?"))
      
      # Drop the rows from wrdcld_data_d that match those terms
      wrdcld_data_d <- filter(wrdcld_data_d, !word %in% remove_terms)
    }
  })
  
  # Build the word cloud
  output$word_cloud <- renderPlot({
    
    # Make sure an access token is present before trying to render anything
    req(access_token())
    
    # Set seed.  Sets base point of random for the randomizing aspects of word cloud generation.
    set.seed("12345")
    
    # Get the data and generate the word cloud
    freq_tbl_data <- create_freq_table()
    
    # Set the color palette to use
    pal2 <- rev(brewer.pal(8,"Spectral")) 
    
    # Generate the word cloud
    wordcloud(freq_tbl_data$word,freq_tbl_data$freq, 
              scale=c(5.5,0.6),
              min.freq=input$min_occurrences,
              max.words=500, 
              random.order=FALSE,
              rot.per=.0,
              colors=pal2)
  })
  
  
  # Build the output for the frequency table
  output$freq_table <- DT::renderDataTable({
    
    freq_table <- create_freq_table()
    
    # Rename the column headings
    colnames(freq_table) <- c("(Stemmed) Term", "Unique Searches")
    
    # Repeat the actual table so we don't just output the column names
    freq_table
    
  },
  rownames = FALSE)
  

  # Output a filtered view of the base data limited to searches.  To minimize false positives and false negatives.
  output$question_searches <- DT::renderDataTable({
    
    ga_data <- get_base_data() %>% 
      filter(grepl("(?i)(^(who|what|why|where|how) )|( (who|what|why|where|how) )", searchKeyword))
    
    # Rename the column headings
    colnames(ga_data) <- c("Question-Like Searches","Unique Searches")
    
    # Repeat the actual table so we don't just output the column names
    ga_data
    
  },
  rownames = FALSE)
  
  ############################
  # Output the base data
  output$raw_data <- DT::renderDataTable({
    
    ga_data <- get_base_data()
    
    # Rename the column headings
    colnames(ga_data) <- c("Search Term","Unique Searches")
    
    # Repeat the actual table so we don't just output the column names
    ga_data
    
  },
  rownames = FALSE)
}