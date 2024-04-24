# minor theme tweaks
fml <- "Lato Light"

pthm <- theme(
  axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
  legend.text = element_text(size = 12), 
  axis.title.y = element_text(size = 12),
  text = element_text(fml), 
  legend.position = 'top',
  # panel.grid.minor=element_blank(),
  # panel.grid.major=element_blank(),
  panel.background = element_rect(fill = '#ECECEC')
) 

make_plot <- function(dataframe, varname, ylabel, station_colname, selsit){
  # dataframe <- epcdata  # dataframe: df to plot
  # varname <- 'chla'  # str: column name of variable to plot
  # ylabel <- 'Concentration (ug/L)'  # str: label for y axis
  # station_colname <- 'epchc_station'  # str: name of column with station ids
  # selsit : str : selected site
  # data to plot
  toplo <- dataframe %>%
    filter(epchc_station %in% selsit) %>% 
    filter(Parameter == varname) %>%
    # filter(yr >= 1975) %>% 
    mutate(
      Date = as.Date(SampleTime),
      ydata = as.numeric(Value)
    ) 

  if(length(toplo) >0 ){
    
    p1 <- ggplot(toplo, aes(x = Date, y = ydata)) + 
      # geom_line(aes(colour = !!sym(varname))) +
      geom_line(colour = "#427355") +
      # scale_colour_manual(values = "#427355") + 
      geom_point(colour = "#427355", size = 0.5) +
      # scale_y_log10() + 
      labs(
        y = ylabel, 
        x = NULL
      ) +
      pthm +
      theme(
        legend.title = element_blank()
      )
    p1 <- ggplotly(p1, dynamicTicks = T)
    # print(p1)
    return(p1)
  } else {
    return("no data")
  }
}
