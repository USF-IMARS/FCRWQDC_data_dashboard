library(glue)
library(ggplot2)
library(dplyr)
library(plotly)

# minor theme tweaks
fml <- "Lato Light"

pthm <- theme(
  axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
  # legend.text = element_text(size = 12), 
  axis.text.y = element_text(size = 11),
  # axis.title.y = element_text(angle = 45),
  
  # plot.title = element_text(hjust = 0.5, vjust = -10),
  text = element_text(fml), 
  legend.position = 'top',
  # panel.grid.minor=element_blank(),
  # panel.grid.major=element_blank(),
  panel.background = element_rect(fill = '#ECECEC')
) 

make_plot <- function(dataframe, varname, ylabel){
  # dataframe <- epcdata  # dataframe: df to plot
  # varname <- 'chla'  # str: column name of variable to plot
  # ylabel <- 'Concentration (ug/L)'  # str: label for y axis
  # assumes site column named "Site"
  # assumes parameter column named "Parameter"

  print(paste("=== DEBUG: make_plot ==="))
  print(paste("Available parameters in data:", paste(unique(dataframe$Parameter), collapse=", ")))

  toplo <- dataframe %>%
    filter(`Parameter` == varname) %>%
    mutate(
      Date = as.Date(datetime),
      ydata = as.numeric(Value)
    )

  print(paste("Rows after filtering for station and parameter:", nrow(toplo)))

  # add a fake row if df is empty to prevent failures
  if(nrow(toplo) < 1 ){  # if no data for this param
    print(paste("WARNING: No data found for parameter:", varname))
    # Create a new row to append
    new_row <- tibble(Date = as.Date("1970-01-01"), ydata = 0)
    # Append the new row to the tibble
    toplo <- bind_rows(toplo, new_row)
  }

  p1 <- ggplot(
    toplo,
    aes(
      x = Date,
      y = ydata,
      text = paste0("Date: ", Date,
                    "<br>", ylabel, ": ", ydata,
                    "<br>Sample Depth: ", `Sample.Depth`))) +
    geom_line(aes(group = 1), colour = "#427355") +
    # scale_colour_manual(values = "#427355") +
    geom_point(colour = "#427355", size = 0.5) +
    # scale_y_log10() +
    labs(
      title = varname,
      y = ylabel,
      x = NULL
    ) +
    pthm
    # theme(
    #   legend.title = element_blank()
    # )
  p1 <- ggplotly(p1, dynamicTicks = TRUE, tooltip="text")
  # print(p1)
  return(p1)
}

