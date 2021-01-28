fluidPage(
  
  
  # title of the dashboard 
  titlePanel(
    h1(
      id = 'title', # the id for the title
      strong('HOLLYWOOD MOST PROFITABLE STORIES DASHBOARD'), # making the title bold
      tags$style(
        HTML('#title{font-size: 3vw; background-color: #969494; color:white;}') # adding some css to the title 
      ) # end of the style tag
    ) # end of the h1 tag
  ), # end of the title panel
  
  
  # sidebar so the user can filter by genre, studio, and year
    sidebarPanel(
      h2('Filter By: '),
      selectInput('yearParameter', 
                  label = h3('Year'), 
                  choices = list('2008' = '2008', '2009' = '2009', '2010' = '2010', '2011' = '2011', 'All' = 'All'),
                  selected = 'All'
      ), # end of the year select input 
      selectInput('genreParameter',
                  label = h3('Genre'),
                  choices = list('Action' = 'Action', 'Animation' = 'Animation', 'Comedy'= 'Comedy', 'Drama' = 'Drama', 
                                 'Fantasy' = 'Fantasy', 'Romance' = 'Romance', 'All' = 'All'),
                  selected = 'All'
      ), # end of the genre select input
      selectInput('studioParameter',
                  label = h3('Studio'),
                  choices = list('20th Century Fox' = '20th Century Fox', 'Fox' = 'Fox', 'Independent' = 'Independent', 
                                 'Universal' = 'Universal', 'Sony' = 'Sony', 'Disney' = 'Disney', 'Lionsgate' = 'Lionsgate', 
                                 'Summit' = 'Summit', 'The Weinstein Company' = 'The Weinstein Company', 'Warner Bros.' = 'Warner Bros.',
                                 'CBS' = 'CBS', 'New Line' = 'New Line', 'Paramount' = 'Paramount', 'All' = 'All'),
                  selected = 'All'
        
      ), # end of the studio select input 
      
      width = 2
    ), # end of the sidebar panel 
  
  
  
  # where the plots go
  mainPanel(
    fluidRow(
      br(),
      column(width = 12, offset = 1, 
        plotOutput("scoreBar", width = '100%'),
        br(),
        br(),
      ), # end of column
      fluidRow(
        column(
          width = 12, 
          splitLayout(cellWidths = c("60%", "40%"), plotOutput('studioBar'),plotOutput('genreBar') # I want both graphs next to each other
          ) 
        )
      )
    )
  )
  
)