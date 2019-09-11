
calendarChart <- function(dates){
  #dat <- read.csv('http://t.co/mN2RgcyQFc')[,c('date', 'pts')]
  dat2 <- transform(dates, date = (as.Date(date)))
  
  years <- as.integer(stringi::stri_sub(dat2$date,1,4)) %>% unique %>% sort
  
  plots <- list()
  for(year in years[1:(length(years)-1)])
  {
    tmp_years <- as.integer(stringi::stri_sub(dat2$date,1,4))
    tmp <- dat2[tmp_years == year,  ]
    
    r1 <- rChartsCalmap::calheatmap(x = 'date', y = 'pts',
                                    height=130,
                                    data = tmp,
                                    #domain = 'year',
                                    start = paste0(year,"-01-01"),
                                    displayLegend = FALSE,
                                    onClick = htmlwidgets::JS('function(date, nb){Shiny.onInputChange("heatmap_date", date);}'),
                                    legend = ceiling(exp(seq(log(1), log(max(dates[,2])-1), length.out = 4))),
                                    itemName = 'hit',
                                    range = 12,
                                    domain = 'month',
                                    subDomain = 'day')
                                   
    plots[[as.character(year)]] <- tagList(tags$h4(year),r1)
  }
  year<-years[length(years)]
  tmp_years <- as.integer(stringi::stri_sub(dat2$date,1,4))
  tmp <- dat2[tmp_years == year,  ]
  
  r1 <- rChartsCalmap::calheatmap(x = 'date', y = 'pts',
                                  height=130,
                                  data = tmp,
                                  #domain = 'year',
                                  start = paste0(year,"-01-01"),
                                  displayLegend = TRUE,
                                  onClick = htmlwidgets::JS('function(date, nb){Shiny.onInputChange("heatmap_date", date);}'),
                                  legend = ceiling(exp(seq(log(1), log(max(dates[,2])-1), length.out = 4))),
                                  itemName = 'hit',
                                  range = 12,
                                  domain = 'month',
                                  subDomain = 'day'
                                                                  )
  plots[[as.character(year)]] <- tagList(tags$h4(year),r1)
  tags <- tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple lineChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    plots
  )
}
