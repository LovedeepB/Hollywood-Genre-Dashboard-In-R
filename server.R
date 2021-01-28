# importing required libraries 
library(ggplot2)
library(reshape2)
library(dplyr)


# where the plots go 
function(input, output) {
  
  
  # data cleaning/ reading in the csv
  get.data <- function(){
    
    # reading in the required library
    holly <- read.csv("HollywoodsMostProfitableStories.csv")
    
    # getting rid of any NA values 
    holly <- holly[complete.cases(holly),]
    holly <- holly[(holly$Lead.Studio != ''),]
    
    # converting values to character 
    holly$Genre <- as.character(holly$Genre)
    holly$Lead.Studio <- as.character(holly$Lead.Studio)
    
    
    # returning the data frame
    return(holly)
  }
  
  
  
  
  
  # PLOT 1: AVERAGE GENRE SCORE 
  output$scoreBar <- renderPlot({
    
    holly <- get.data()
    
    # create variables that hold the filters (genre, year, and studio)
    year.parameter <- input$yearParameter
    genre.parameter <- input$genreParameter
    studio.parameter <- input$studioParameter
    
    
    # YEAR FILTER
    year.label = year.parameter
    # if statements to filter the data by
    if(year.parameter == '2008'){
      holly <- holly[holly$Year == 2008,]
    }
    else if(year.parameter == '2009'){
      holly <- holly[holly$Year == 2009,]
    }
    else if(year.parameter == '2010'){
      holly <- holly[holly$Year == 2010,]
    }
    else if(year.parameter == '2011'){
      holly <- holly[holly$Year == 2011,]
    }
    else{
      year.label = '2008 Till 2011'
    }
    
  
    
    # GENRE FILTER 
    if(genre.parameter == 'Action'){
      holly <- holly[holly$Genre == 'Action',]
    }
    else if(genre.parameter == 'Animation'){
      holly <- holly[holly$Genre == 'Animation',]
    }
    else if(genre.parameter == 'Comedy'){
      holly <- holly[holly$Genre == 'Comedy',]
    }
    else if(genre.parameter == 'Drama'){
      holly <- holly[holly$Genre == 'Drama',]
    }
    else if(genre.parameter == 'Fantasy'){
      holly <- holly[holly$Genre == 'Fantasy',]
    }
    else if(genre.parameter == 'Romance'){
      holly <- holly[holly$Genre == 'Romance',]
    }
    
    
    
    # STUDIO FILTER
    if(studio.parameter == '20th Century Fox'){
      holly <- holly[holly$Lead.Studio == '20th Century Fox',]
    }
    else if(studio.parameter == 'Fox'){
      holly <- holly[holly$Lead.Studio == 'Fox',]
    }
    else if(studio.parameter == 'Independent'){
      holly <- holly[holly$Lead.Studio == 'Independent',]
    }
    else if(studio.parameter == 'Universal'){
      holly <- holly[holly$Lead.Studio == 'Universal',]
    }
    else if(studio.parameter == 'Sony'){
      holly <- holly[holly$Lead.Studio == 'Sony',]
    }
    else if(studio.parameter == 'Disney'){
      holly <- holly[holly$Lead.Studio == 'Disney',]
    }
    else if(studio.parameter == 'Lionsgate'){
      holly <- holly[holly$Lead.Studio == 'Lionsgate',]
    }
    else if(studio.parameter == 'Summit'){
      holly <- holly[holly$Lead.Studio == 'Summit',]
    }
    else if(studio.parameter == 'The Weinstein Company'){
      holly <- holly[holly$Lead.Studio == 'The Weinstein Company',]
    }
    else if(studio.parameter == 'Warner Bros.'){
      holly <- holly[holly$Lead.Studio == 'Warner Bros.',]
    }
    else if(studio.parameter == 'CBS'){
      holly <- holly[holly$Lead.Studio == 'CBS',]
    }
    else if(studio.parameter == 'New Line'){
      holly <- holly[holly$Lead.Studio == 'New Line',]
    }
    else if(studio.parameter == 'Paramount'){
      holly <- holly[holly$Lead.Studio == 'Paramount',]
    }
    
    # finding the mean of the audience score and the rotten tomatoes score for the genre
    holly %>%
      group_by(Genre) %>%
      summarise_at(.vars = vars(Audience..score.., Rotten.Tomatoes..), .funs = c('mean')) -> temp.holly
    
    
    
    # convert genre values to numbers
    temp.holly$Genre[temp.holly$Genre == 'Action'] <- '1'
    temp.holly$Genre[temp.holly$Genre == 'Animation'] <- '2'
    temp.holly$Genre[temp.holly$Genre == 'Comedy'] <- '3'
    temp.holly$Genre[temp.holly$Genre == 'Drama'] <- '4'
    temp.holly$Genre[temp.holly$Genre == 'Fantasy'] <- '5'
    temp.holly$Genre[temp.holly$Genre == 'Romance'] <- '6'
    # convert the type of genre to numeric
    temp.holly$Genre <- as.numeric(temp.holly$Genre)
    
    
    
    # if there is no data to be printed then print this statement out 
    validate(
      need(nrow(temp.holly) != 0, 'No ratings exist for this')
    )
    
    
    # now get the mean of the audience score and rotten tomatoes score based on genre
    temp.holly.mean <- aggregate(temp.holly, by = list(temp.holly$Genre), mean)

        
    # get rid of Group 1 column 
    temp.holly.mean <- subset(temp.holly.mean, select = -c(Group.1))
    
    
    # reformat the data
    temp.holly.mean <- melt(temp.holly.mean, id.vars = 'Genre')
    
    
    # convert the labels back for the genre
    temp.holly.mean$Genre[temp.holly.mean$Genre == '1'] <- 'Action'
    temp.holly.mean$Genre[temp.holly.mean$Genre == '2'] <- 'Animation'
    temp.holly.mean$Genre[temp.holly.mean$Genre == '3'] <- 'Comedy'
    temp.holly.mean$Genre[temp.holly.mean$Genre == '4'] <- 'Drama'
    temp.holly.mean$Genre[temp.holly.mean$Genre == '5'] <- 'Fantasy'
    temp.holly.mean$Genre[temp.holly.mean$Genre == '6'] <- 'Romance'
    
    
    # convert variable type to character 
    temp.holly.mean$variable <- as.character(temp.holly.mean$variable)
    
    
    # edit the variable information 
    temp.holly.mean$variable[temp.holly.mean$variable == 'Audience..score..']<- 'Audience Score'
    temp.holly.mean$variable[temp.holly.mean$variable == 'Rotten.Tomatoes..']<- 'Rotten Tomatoes'
    
    
    
    # chart title 
    chart.title <- paste('Average Genre Score From', year.label)
    
    
    # now plot the information 
    ggplot(temp.holly.mean, aes(Genre, value, fill = variable))+ 
      geom_bar(
        stat = 'identity', 
        position = 'dodge'
      )+ # end of geom_bar
      scale_fill_manual(
        values = c('#a9c9c2', '#37A6A0') # creating the color palette for the bars 
      )+ # end of the scale_fill_manual 
      geom_text(
        aes(label = round(value,1)), # only show 1 decimal point of the score
        position = position_dodge(width = 0.9),
        vjust = 0.25,
        hjust = 2
      )+# end of the geom_text
      ggtitle(chart.title)+
      coord_flip()+ # flipping the axis
      scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # move the bars closer to the labels 
      theme(
        panel.grid.major = element_blank(), # get rid of the major lines
        panel.grid.minor = element_blank(), # get rid of the minor lines 
        plot.title = element_text(
          size = 25, # increase the size
          hjust = 0.5, # this and vjust help center the title
          vjust = 2, 
          face = 'bold' # make the title bold
        ), 
        panel.background = element_rect( # make the background white
          fill = 'white'
        ),
        axis.title.x = element_blank(), # remove the x axis label 
        axis.text.x = element_blank(), # get rid of the x label text
        axis.title.y = element_blank(), # remove the y axis label
        axis.text.y = element_text(size = 12), # increase size of the genre titles on the y axis
        axis.ticks = element_blank(), # remove the ticks
        legend.title = element_blank(), # remove the legend title
        legend.text = element_text(
          size = 11, # editing the size of the legend text 
        ),
        legend.position = 'top', # position of the legend 
      ) # end of the theme 
    
  }) # end of plot 1
  
  
  
  
  
  
  
  
  
  
  
  
  # PLOT 2: studio vs profit
  output$studioBar <- renderPlot({
    # getting the data
    holly <- get.data()
    
    
    # create variables that hold the filters (genre, year, and studio)
    year.parameter <- input$yearParameter
    genre.parameter <- input$genreParameter
    studio.parameter <- input$studioParameter
    
    
    # YEAR FILTER
    if(year.parameter == '2008'){
      holly <- holly[holly$Year == 2008,]
    }
    else if(year.parameter == '2009'){
      holly <- holly[holly$Year == 2009,]
    }
    else if(year.parameter == '2010'){
      holly <- holly[holly$Year == 2010,]
    }
    else if(year.parameter == '2011'){
      holly <- holly[holly$Year == 2011,]
    }
    
    
    
    # GENRE FILTER 
    if(genre.parameter == 'Action'){
      holly <- holly[holly$Genre == 'Action',]
    }
    else if(genre.parameter == 'Animation'){
      holly <- holly[holly$Genre == 'Animation',]
    }
    else if(genre.parameter == 'Comedy'){
      holly <- holly[holly$Genre == 'Comedy',]
    }
    else if(genre.parameter == 'Drama'){
      holly <- holly[holly$Genre == 'Drama',]
    }
    else if(genre.parameter == 'Fantasy'){
      holly <- holly[holly$Genre == 'Fantasy',]
    }
    else if(genre.parameter == 'Romance'){
      holly <- holly[holly$Genre == 'Romance',]
    }
    
    
    studio.label = 'Studios'
    # STUDIO FILTER
    if(studio.parameter == '20th Century Fox'){
      holly <- holly[holly$Lead.Studio == '20th Century Fox',]
      studio.label = '20th Century Fox Studio'
    }
    else if(studio.parameter == 'Fox'){
      holly <- holly[holly$Lead.Studio == 'Fox',]
      studio.label = 'Fox Studio'
    }
    else if(studio.parameter == 'Independent'){
      holly <- holly[holly$Lead.Studio == 'Independent',]
      studio.label = 'Independent Studio'
    }
    else if(studio.parameter == 'Universal'){
      holly <- holly[holly$Lead.Studio == 'Universal',]
      studio.label = 'Universal Studio'
    }
    else if(studio.parameter == 'Sony'){
      holly <- holly[holly$Lead.Studio == 'Sony',]
      studio.label = 'Sony Studio'
    }
    else if(studio.parameter == 'Disney'){
      holly <- holly[holly$Lead.Studio == 'Disney',]
      studio.label = 'Disney Studio'
    }
    else if(studio.parameter == 'Lionsgate'){
      holly <- holly[holly$Lead.Studio == 'Lionsgate',]
      studio.label = 'Lionsgate Studio'
    }
    else if(studio.parameter == 'Summit'){
      holly <- holly[holly$Lead.Studio == 'Summit',]
      studio.label = 'Summit Studio'
    }
    else if(studio.parameter == 'The Weinstein Company'){
      holly <- holly[holly$Lead.Studio == 'The Weinstein Company',]
      studio.label = 'The Weinstein Company Studio'
    }
    else if(studio.parameter == 'Warner Bros.'){
      holly <- holly[holly$Lead.Studio == 'Warner Bros.',]
      studio.label = 'Warner Bros. Studio'
    }
    else if(studio.parameter == 'CBS'){
      holly <- holly[holly$Lead.Studio == 'CBS',]
      studio.label = 'CBS Studio'
    }
    else if(studio.parameter == 'New Line'){
      holly <- holly[holly$Lead.Studio == 'New Line',]
      studio.label = 'New Line Studio'
    }
    else if(studio.parameter == 'Paramount'){
      holly <- holly[holly$Lead.Studio == 'Paramount',]
      studio.label = 'Paramount Studio'
    }
    
    
    
    # getting the data that I need to plot
    vars = c('Lead.Studio', 'Profitability')
    temp.holly <- holly[vars]
    
    # chart title 
    chart.title = paste(studio.label,'VS. Profitability')
    
    
    # grouping by the studio and finding the sum of the profit
    temp.holly %>% 
      group_by(Lead.Studio) %>%
      summarise(Profitability = sum(Profitability)) -> temp.holly
    
    
    # if there is no data to be printed then print this statement out 
    validate(
      need(nrow(temp.holly) != 0, 'No profits exist for this')
    )
    
    
    # plot the chart
    ggplot(temp.holly, aes(Lead.Studio, Profitability))+ # use these attributes as your X and Y
      geom_bar(stat = 'identity', fill = '#90CDD0')+ # make a bar chart
      geom_text(
        aes(label = round(Profitability, 1)),
        position = position_dodge(width = 0.9),
        hjust = -0.1
      )+ # end of geom_text
      ggtitle(chart.title)+ # chart title
      coord_flip()+ # flip the x and y
      scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # move the bars closer to the label
      theme(
        axis.title.x = element_blank(), # getting rid of the x label
        axis.text.x = element_blank(), # getting rid of the x values
        axis.title.y = element_blank(), # getting rid of the y label
        axis.text.y = element_text( # increasing the size of the studios names
          size = 12
        ),
        axis.ticks = element_blank(), # getting rid of the axis ticks
        panel.grid.major = element_blank(), # getting rid of the grid lines
        panel.grid.minor = element_blank(), # getting rid of the grid lines
        panel.background = element_rect( # make the background white
          fill = 'white'
        ),
        plot.title = element_text( # editing the title size, weight, and location
          size = 18, 
          face = 'bold',
          hjust = 0.5
        )
      ) # end of the theme block
    
  }) # end of plot 2
  
  
  
  
  
  
  
  
  # PLOT 3: Genre vs. world wide gross
  output$genreBar <- renderPlot({
    # getting the data
    holly <- get.data()
    
    
    # create variables that hold the filters (genre, year, and studio)
    year.parameter <- input$yearParameter
    genre.parameter <- input$genreParameter
    studio.parameter <- input$studioParameter
    
    # YEAR FILTER
    if(year.parameter == '2008'){
      holly <- holly[holly$Year == 2008,]
    }
    else if(year.parameter == '2009'){
      holly <- holly[holly$Year == 2009,]
    }
    else if(year.parameter == '2010'){
      holly <- holly[holly$Year == 2010,]
    }
    else if(year.parameter == '2011'){
      holly <- holly[holly$Year == 2011,]
    }
    
    
    genre.label = 'Genre'
    
    # GENRE FILTER 
    if(genre.parameter == 'Action'){
      holly <- holly[holly$Genre == 'Action',]
      genre.label = 'Action Genre'
    }
    else if(genre.parameter == 'Animation'){
      holly <- holly[holly$Genre == 'Animation',]
      genre.label = 'Animation Genre'
    }
    else if(genre.parameter == 'Comedy'){
      holly <- holly[holly$Genre == 'Comedy',]
      genre.label = 'Comedy Genre'
    }
    else if(genre.parameter == 'Drama'){
      holly <- holly[holly$Genre == 'Drama',]
      genre.label = 'Drama Genre'
    }
    else if(genre.parameter == 'Fantasy'){
      holly <- holly[holly$Genre == 'Fantasy',]
      genre.label = 'Fantasy Genre'
    }
    else if(genre.parameter == 'Romance'){
      holly <- holly[holly$Genre == 'Romance',]
      genre.label = 'Romance Genre'
    }
    
    
    
    # STUDIO FILTER
    if(studio.parameter == '20th Century Fox'){
      holly <- holly[holly$Lead.Studio == '20th Century Fox',]
    }
    else if(studio.parameter == 'Fox'){
      holly <- holly[holly$Lead.Studio == 'Fox',]
    }
    else if(studio.parameter == 'Independent'){
      holly <- holly[holly$Lead.Studio == 'Independent',]
    }
    else if(studio.parameter == 'Universal'){
      holly <- holly[holly$Lead.Studio == 'Universal',]
    }
    else if(studio.parameter == 'Sony'){
      holly <- holly[holly$Lead.Studio == 'Sony',]
    }
    else if(studio.parameter == 'Disney'){
      holly <- holly[holly$Lead.Studio == 'Disney',]
    }
    else if(studio.parameter == 'Lionsgate'){
      holly <- holly[holly$Lead.Studio == 'Lionsgate',]
    }
    else if(studio.parameter == 'Summit'){
      holly <- holly[holly$Lead.Studio == 'Summit',]
    }
    else if(studio.parameter == 'The Weinstein Company'){
      holly <- holly[holly$Lead.Studio == 'The Weinstein Company',]
    }
    else if(studio.parameter == 'Warner Bros.'){
      holly <- holly[holly$Lead.Studio == 'Warner Bros.',]
    }
    else if(studio.parameter == 'CBS'){
      holly <- holly[holly$Lead.Studio == 'CBS',]
    }
    else if(studio.parameter == 'New Line'){
      holly <- holly[holly$Lead.Studio == 'New Line',]
    }
    else if(studio.parameter == 'Paramount'){
      holly <- holly[holly$Lead.Studio == 'Paramount',]
    }
    
    
    
    # creating the temporary dataframe that hold the data that will be shown in the plot
    vars <- c('Genre', 'Worldwide.Gross')
    temp.holly <- holly[vars]
    
    # chart title
    chart.title = paste(genre.label, 'VS. Worldwide Gross')
    
    # grouping and finding the sum of the worldwide gross
    temp.holly %>% 
      group_by(Genre) %>% # grouping by the genre
      summarise(Worldwide.Gross = sum(Worldwide.Gross)) -> temp.holly
    
    
    # if there is no data to be printed then print this statement out 
    validate(
      need(nrow(temp.holly) != 0, 'No world wide gross exist for this')
    )
      

    # plot the chart
    ggplot(temp.holly, aes(Genre, Worldwide.Gross)) + # the attributes to be plotted
      geom_bar(stat = 'identity', fill = '#87BDD6')+ # creating a bar chart and adding the fill to the bars
      ggtitle(chart.title)+ # adding the title to the plot
      geom_text(
        aes(label = round(Worldwide.Gross, 1)),
        position = position_dodge(width = 0.9),
        hjust = -0.1
      )+ # adding the data labels to the bars
      coord_flip()+ # flipping the x and y
      scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # move the bars closer to the label
      theme(
        axis.title = element_blank(), # getting rid of the label titles
        axis.ticks = element_blank(), # getting rid of the ticks
        axis.text.x = element_blank(), # getting rid of the x values
        axis.text.y = element_text( # increasing the size of the y values
          size = 12
        ), 
        panel.background = element_rect( # making the plot background be the same as the page background
          fill = 'white'
        ), 
        panel.grid.major = element_blank(), # getting rid of the lines
        panel.grid.minor = element_blank(), # getting rid of the lines 
        plot.title = element_text( # editing the title size, weight, and location
          size = 18, 
          face = 'bold',
          hjust = 0.5
        )
      ) # end of the theme block
    
    
  })
  
  
  
  
}
