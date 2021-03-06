######################
# Define the UI
######################
library(googleAuthR)
library(googleAnalyticsR)
library(shiny)

ui <- fluidPage(
  
  # So you can track stuff.  You can also just put the google analytics script in here- just go to your admin in google analytics and 
  # then go to your property and create a new property (to get a new UA tracking ID) and after you are done doing that it will give you 
  # a script at the end, copy the whole script and use notepad++ or some kind of text editor/VIM to save a .js file.  This is IMPORTANT
  # though- make sure to remove the script tags the <script> opening tag and the </script> closing tag from your ga.js file.  whatever you
  # save your filename as- replace the "gtm.js" with whatever name you name it followed by .js - denoting that its a javascript file so that
  # this R script can run it.  This line below is already running code to ensure it runs as a script so very important on how and where you
  # save this.  Save it exactly in your working directory i.e. the file that your project is in before you edit this line of code on line
  # 18 here and then go ahead and deploy.  Then go back to your analytics and check to see if its working- because at that point- youll
  # be able to see engagements and use of your app/tool.  You can also use google tag manager of course. Same thing goes- create a 
  # container, and then after you've added your UA/analytics tag for page views/events then save it and get the script with the functions
  # at the end and remove the opening and closing script tags and save it in your project file and you can just save either or as just
  # "gtm.js" to save you the hassle of one extra step of renaming this.  :)
  tags$head(includeScript("gtm.js")),
  
  theme = "cosmo",   # It's definetely a cosmo-ish feel and them.  Go for it.  This is simply for styling, chill...
  
  # Title and Overview
  titlePanel("Google Analytics Site Search Term Viewer"),
  "Log in and select a view to explore the individual search terms and how often they",
  "get used on your site. This effort was inspired by Tim Wilson of Analytics Demystified (and cribbed from) work initially",
  "created by ", tags$a(href="https://www.linkedin.com/pulse/using-r-get-real-insights-from-your-searched-terms-words-brodeur",
                        target="_blank","Sébastien Brodeur"),
  ", with some additional inspiration from ",
  tags$a(href="https://nanalytics.wordpress.com/2014/07/14/who-what-where-when-why-how-harnessing-the-power-of-internal-site-search/",
         target="_blank","Nancy Koons"),". The source code for this application is available",
  tags$a(href="https://github.com/gilliganondata/site-search-wordcloud", target="_blank", "on Github"),".",
  
  tags$hr(),
  
  # Sidebar with the user-controllable inputs 
  sidebarLayout(
    sidebarPanel(
      
      # A bit of a hack, I suspect, but just centering the login button. And. the log OUT button
      # still winds up not centered. :-(
      tags$div(style="text-align: center;",
               
               # Get the user to log in and then select a view. This uses the JS version
               # because it seems to be less finicky.
               gar_auth_jsUI("auth_module", login_text = "Login with Google")
      ),
      
      # Get the account/property/view
      authDropdownUI("auth_menu"),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # The date range dropdown, including a default value
      selectInput("daterange", label = "Select a date range:", 
                  choices = daterange_options, 
                  selected = 30),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # Select the minimum number of occurrences to include
      sliderInput("min_occurrences",
                  label = "Adjust the minimum number of occurrences to include in the word cloud:",
                  min = 1,
                  max = 10,
                  value = 3),
      
      # Slider setting
      verbatimTextOutput("slider_setting"),
      
      # Terms to remove. This is so a non-stopword (like the the brand name) can
      # be entered and then removed from the wordcloud so it doesn't dominate.
      textInput("exclude_terms", label = paste("Enter (stemmed) terms separated by commas",
                                               "to remove them from the word cloud and frequency table."), 
                value = NULL)
    ),
    
    # All of the actual output. This is just four tabs, which could easily be added onto if
    # other ways of exploring this same data are conceived. The bulk of the "code" for this
    # is just getting the descriptive text into each tab.
    mainPanel(
      tags$h4("Site Search Term Usage"),
      
      tabsetPanel(
        tabPanel("Word Cloud", tags$br(), "To remove specific terms from the word cloud, enter them in the",
                 "box in the sidebar to the left (this will remove the words from both the word cloud ",
                 "and the ", tags$strong("Frequency Table"), ".", tags$br(), " ",
                 plotOutput("word_cloud")),
        tabPanel("Frequency Table", tags$br(),
                 "The table below shows the frequency of each individual keyword (stemmed) ",
                 "that was included in searches. To remove specific terms from the frequency table, enter",
                 "them in the box in the sidebar to the left (this will remove the words from both the",
                 "frequency table and the", tags$strong("Word Cloud"), ".", tags$p(" "),
                 DT::dataTableOutput("freq_table")),
        tabPanel("Questions in Search", tags$br(),
                 "The table below shows (generally long-tail) searches that were likely ",
                 "specific questions being asked in the search box. These can provide insight ",
                 "as to very specific things for which visitors are looking for content. This ",
                 "way of exploring search data comes straight from ",
                 tags$a(href="https://nanalytics.wordpress.com/2014/07/14/who-what-where-when-why-how-harnessing-the-power-of-internal-site-search/",
                        target="_blank","Nancy Koons"), ".", tags$p(" "),
                 DT::dataTableOutput("question_searches")),
        tabPanel("Raw Google Analytics Results", tags$br(),
                 "The table below shows the search data as it was pulled from Google Analytics. ",
                 "This would match what you see if you view the ", tags$strong("Search Terms"), " report for the ",
                 "same view and same timeframe in Google Analytics.", tags$p(" "),
                 DT::dataTableOutput("raw_data"))
      )
    )
  )
)

