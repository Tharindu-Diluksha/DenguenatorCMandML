#require(grid)
#require(gridExtra)
for(area in areas) {
  year = 2012
  mohName = area
  frame = results[results$moh_name==mohName & results$year==year,]
  
  shPlot<- ggplot(data = data.frame(week = frame$day, value = frame$best.sh),
                  aes(x = week, y = value)) +
    xlab("Week") +
    ylab("Sh Value") +
    #theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_point(alpha=0.3)
  #ggtitle(title, subtitle = subtitle) +
  #theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  ehPlot<- ggplot(data = data.frame(week = frame$day, value = frame$best.eh),
                  aes(x = week, y = value)) +
    xlab("Week") +
    ylab("Eh Value") +
    #theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_point(alpha=0.3)
  #ggtitle(title, subtitle = subtitle) +
  #theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  ihPlot<- ggplot(data = data.frame(week = frame$day, value = frame$best.ih),
                  aes(x = week, y = value)) +
    xlab("Week") +
    ylab("Ih Value") +
    #theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_point(alpha=0.3)
  #ggtitle(title, subtitle = subtitle) +
  #theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  rhPlot<- ggplot(data = data.frame(week = frame$day, value = frame$best.rh),
                  aes(x = week, y = value)) +
    xlab("Week") +
    ylab("Rh Value") +
    #theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_point(alpha=0.3)
  #ggtitle(title, subtitle = subtitle) +
  #theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  
  ihPlotInARange<- ggplot(data = data.frame(week = frame$day[frame$day>2], value = frame$best.ih[frame$day>2]),
                          aes(x = week, y = value)) +
    xlab("Week") +
    ylab("Ih Value") +
    #theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_point(alpha=0.3)
  #ggtitle(title, subtitle = subtitle) +
  
  
  multiplot(shPlot, ehPlot, ihPlotInARange, rhPlot, cols = 2, main = paste(mohName, " - ", year))
  
}


year = 2012
ihVsWeek2012 = plotIhVsWeeks("MC - Colombo", results[results$year==year,]$day, results[results$year==year,]$best.ih, year, withYaxis = T)
year = 2013
ihVsWeek2013 = plotIhVsWeeks("MC - Colombo", results[results$year==year,]$day, results[results$year==year,]$best.ih, year, withYaxis = T)
multiplot2(ihVsWeek2012, ihVsWeek2013, cols = 1)