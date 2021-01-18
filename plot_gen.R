library(partitions)
library(dplyr)
library(TraMineR)
library(randomcoloR)

integer_plot <- function(x,y, plot = T){
  
  if(y == 'd' & plot == T){
    
    partitions::diffparts(x) %>%
      as.matrix() %>%
      # t %>%
      seqdef() %>%
      seqIplot(.,
               with.legend = F,
               yaxis = F,
               xaxis = F,
               ylab = NA,
               cpal = colorRampPalette(c("#ffcc66", "#ff3399"))(x))
  }
  
  if(y == 'p' & plot == T){
    partitions::parts(x) %>%
      as.matrix() %>%
      # t %>%
      seqdef() %>%
      seqIplot(.,
               with.legend = F,
               yaxis = F,
               xaxis = F,
               ylab = NA,
               cpal = colorRampPalette(c("#ffcc66", "#ff3399"))(x))
  }
  
  if(y == 'p' & plot == F){
    out <- partitions::parts(x) %>%
      as.matrix() %>%
      # t %>%
      seqdef()
    
      return(out)
  }
  
  if(y == 'd' & plot == F){
    out <- partitions::diffparts(x) %>%
      as.matrix() %>%
      # t %>%
      seqdef()
    
      return(out)
  }
  
}

par(mai = c(0.1,0.1,0.1,0.1),
    oma = c(2,2,2,2),
    xpd = NA,
    bg = 'black')

sapply(c(3:15), integer_plot, y = 'd')

par(mfrow = c(3,3), 
    mai = c(0.1,0.1,0.1,0.1),
    oma = c(2,2,2,2),
    xpd = NA,
    bg = 'black')

sapply(c(3:11), integer_plot, y = 'p')
