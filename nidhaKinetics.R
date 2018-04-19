data <- read.csv('kineticData.csv')
library(dplyr)
library(gridExtra)
library(ggplot2)

dev.off()

data['absorbance'] = log10(100/data$transmittance)
data['concentration'] = data$absorbance / 111.9

data.no.outliers <- data %>%
  filter(
    !(run == 2 & time == 180) &
    !(run == 3 & time == 30) &
    !(run == 4 & time == 510)
  )

data <- data.no.outliers

create.plot <- function(run_id) {
  df <- data %>%
    filter(run == run_id) %>%
    select(time, concentration)

  m = (df$concentration[2] - df$concentration[1]) / (df$time[2] - df$time[1])
  b = df$concentration[1]
  
  plot1 <- ggplot(df) + 
    geom_point(aes(time, concentration)) +
    geom_abline(intercept = b, slope=m) +
    xlab('Time (s)') +
    ylab('[I3-] (M)') + 
    ggtitle(paste('Run ', run_id, ': [I3-] vs. time (slope: ', format(m, digits=6), ')'))
        
  return(plot1)
}

p1 <- create.plot(1)
p2 <- create.plot(2)
p3 <- create.plot(3)
p4 <- create.plot(4)

grid.arrange(p1, p2, p3, p4)

