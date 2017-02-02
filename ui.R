## JMJPFU ##
# 7 Feb 2016 # With my Lords help, I am attempting the first set of visualisation for the Movie Sentiment Analyser

fluidPage(
  
  ## The below is to suppress any error messages on to the screen
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  ## End of error message suppressing part ########
  
  # Application Title
 
  
   titlePanel(title=h2("Movie Reviews Sentiment Analyzer",align="center")), # This will display the title with alignment to center
  
  
  
  sidebarLayout(
    # Side bar panel with options to select language and then movies according to the language
    # selected
    sidebarPanel(
      selectInput("langsel","Choose Your Language:",choices=list("English"="eng","Hindi"="hind")), # This will go as drop down in the app. 
      
      br(), # A break in the panel
      
      uiOutput("movie_list"), # This list of movies will  come from renderUI of server based on selection of language
      
      br(),
      
      helpText("The below upload utility is to upload your own text and get predictions and sentiment analysis based on the uploaded text. More the text, better the prediction. Please upload a text file"),
      br(),
#       helpText("A sample template can be downloaded by clicking on the download button"),
#       br(),
      
#       downloadButton('downloadtemplate','Download'),
#       
#       br(),
      
      fileInput("movtext","Upload Your Movie Reviews"), # The fileinput() function is used to get the fileupload option
      
      helpText("Default File Size is 5 MB"), # A help text 
      br(),
      
      helpText("The text you upload, produces an absolute decision rating,positive and negative word clouds and also percentage of positive and negative terms in the text"),
      
      tags$style("body{background-color:linen;color:maroon;font-size:18px;font-family:Tahoma}"),
      
      tags$style(".shiny-bound-output{font-size:20px;font-style:bold;font-family:Tahoma;font-weight:500;color:maroon}")
      
      
      
      
        ), # End of sidebarpanel
    
    # Show the ratings
    mainPanel(
      
      
            tabsetPanel(type="tab",
                        
                        
                        tabPanel("Rating",fluidRow(column(6,align="center",offset=3,htmlOutput("rating")),column(12,uiOutput("textrating1"),align="center"),column(6,plotOutput("wordplot_pos")),column(6,plotOutput("wordplot_neg")),column(12,align="center",htmlOutput("wordplot_mov")))), # This is for storing the ratings
                        
                        
                        
                        #tabPanel("Plot",plotOutput("classplot")), # This is for storing the plot
                        tabPanel("Prediction",fluidRow(column(6,align="center",offset=3 ,htmlOutput("googview")),column(12,uiOutput("textrating"),align="center"),column(6,plotOutput("wordplot2")),column(6,plotOutput("wordplot3")),column(12,align="center",htmlOutput("wordplot4")))) # This is for sentiment analysis of text
                        
                        #,column(4,uiOutput("textrating"))
                        ) ## End of the tabset panel function
      
             
              
              ) # End of the mainpanel 
    
    
    
  ) # End of sidebar layout
  
  
  
  
  
  
  
  
  
) # End of fluid page