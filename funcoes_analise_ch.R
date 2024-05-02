#function to create dataframe
create_dataframe <- function(columm, gap) {
  df <- data.frame(matrix(ncol = columm, nrow = 0))
  n <- seq(1,columm,1)
  n[1] = 'session'
  for (i in 2:columm) {
    n[i] = paste('time',gap*(i-1))
  }
  names(df) = n
  return(df)
}

#function to fill NA values
fill_na <- function(data, rows, datapop) {
  for (i in 1:rows) {
    start = FALSE
    end = datapop %>% filter(session == data[i,1])
    end <- ceiling(max(end$time)/gap+1)
    for (j in 2:length(data)){
      if (j <= end){
        if (is.na(data[i,j])){
          if (start){
            data[i,j] <- data[i, j-1]
          }else{
            data[i,j] <- 0
          }
        }else{
          start = TRUE
        }
      }else{
        data[i,j] <- 0
      }
    }
  }
  return(data)
}

#function that combines the series and performs the clustering
return_cluster_comb <- function(data_serie1, data_serie2, data_serie3, sessions, num_k, cols) {
  series1 <- data_serie1[,1:cols] %>% filter(session %in% sessions)
  series2 <- data_serie2[,1:cols] %>% filter(session %in% sessions)
  series3 <- data_serie3[,1:cols] %>% filter(session %in% sessions)
  #series.pop.comb <- merge(series1, series2, by.x = "session", by.y = "session")
  #series.pop.comb <- merge(series.pop.comb, series3, by.x = "session", by.y = "session")
  series.pop.comb <- merge(series1, series2, by.x = "session", by.y = "session", all=TRUE)
  series.pop.comb <- merge(series.pop.comb, series3, by.x = "session", by.y = "session", all=TRUE)
  series.pop.comb[is.na(series.pop.comb)] <- 0
  series.pop.comb$k <- fit.cluster_kmeans(cluster_kmeans(k=num_k), series.pop.comb[,2:length((series.pop.comb))])
  return(series.pop.comb)
}

#function for line plots
line_cluster_sep <- function(data_serie, gap, cols, title1, title2, title3, linf1, lsup1, linf2, lsup2, linf3, lsup3) {
  #separating the series
  end_serie1 <- cols
  end_serie2 <- (2*cols)-1
  end_serie3 <- (3*cols)-2
  #serie1 <- data_serie[,2:61]
  serie1 <- data_serie[,2:end_serie1]
  serie1$k <- data_serie$k
  #serie2 <- data_serie[,62:121]
  serie2 <- data_serie[,(end_serie1+1):(end_serie2)]
  serie2$k <- data_serie$k
  #serie3 <- data_serie[,122:181]
  serie3 <- data_serie[,(end_serie2+1):(end_serie3)]
  serie3$k <- data_serie$k
  
  #calculating the averages
  mean.serie1 <- as.data.frame(serie1 %>% group_by(k)%>% summarise(across(everything(), list(mean))))
  mean.serie2 <- as.data.frame(serie2 %>% group_by(k)%>% summarise(across(everything(), list(mean))))
  mean.serie3 <- as.data.frame(serie3 %>% group_by(k)%>% summarise(across(everything(), list(mean))))
  
  #end x axis
  end_x <- (cols-1)*10
  
  #data plot 1
  data_plot1 <- melt(mean.serie1, id.vars = c(1))
  data_plot1 <- data_plot1[order(data_plot1$k, data_plot1$variable),]
  #creating x axis
  data_plot1$x <- seq(from = 10, to = end_x, by = gap)
  
  #data plot 2
  data_plot2 <- melt(mean.serie2, id.vars = c(1))
  data_plot2 <- data_plot2[order(data_plot2$k, data_plot2$variable),]
  #creating x axis
  data_plot2$x <- seq(from = 10, to = end_x, by = gap)
  
  #data plot 3
  data_plot3 <- melt(mean.serie3, id.vars = c(1))
  data_plot3 <- data_plot3[order(data_plot3$k, data_plot3$variable),]
  #creating x axis
  data_plot3$x <- seq(from = 10, to = end_x, by = gap)
  
  #plot
  p1 <- ggplot(data_plot1, aes(x=x, y=value, group=k)) +
    geom_hline(yintercept =  2, linetype="dashed", color = "grey80", size=.7) + 
    geom_hline(yintercept =  6, linetype="dashed", color = "grey80", size=.7) +
    scale_color_brewer(palette="Dark2")+
    geom_line(aes(color=as.factor(k)), size = 1.25) +
    #ggtitle(title1)
    labs(title=title1,x ="Time", y = "Quantity", color = "K")+
    scale_y_continuous(limits = c(linf1, lsup1))+#, breaks = c(15,30), labels = c("20", "40")) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.line = element_line(size = 2, colour = "grey80"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"),
          legend.text = element_text(size=14))
  #legend.position = "bottom")
  
  p2 <- ggplot(data_plot2, aes(x=x, y=value, group=k)) +
    geom_hline(yintercept =  5, linetype="dashed", color = "grey80", size=.7) + 
    geom_hline(yintercept =  10, linetype="dashed", color = "grey80", size=.7) +
    scale_color_brewer(palette="Dark2")+
    geom_line(aes(color=as.factor(k)), size = 1.25) +
    #ggtitle(title2)
    labs(title=title2,x ="Time", y = "Quantity", color = "K")+
    scale_y_continuous(limits = c(linf2, lsup2))+#, breaks = c(15,30), labels = c("20", "40")) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.line = element_line(size = 2, colour = "grey80"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"),
          legend.text = element_text(size=14))
  
  
  p3 <- ggplot(data_plot3, aes(x=x, y=value, group=k)) +
    geom_hline(yintercept =  5, linetype="dashed", color = "grey80", size=.7) + 
    geom_hline(yintercept =  10, linetype="dashed", color = "grey80", size=.7) +
    scale_color_brewer(palette="Dark2")+
    geom_line(aes(color=as.factor(k)), size = 1.25) +
    #ggtitle(title3)
    labs(title=title3,x ="Time", y = "Quantity", color = "K")+
    scale_y_continuous(limits = c(linf3, lsup3))+#, breaks = c(15,30), labels = c("20", "40")) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.line = element_line(size = 2, colour = "grey80"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"),
          legend.text = element_text(size=14))
  
  g = grid.arrange(p1, p2, p3, nrow=3)
  return(g)
}

############################################################
#### Função de preencher valores ausentes versão score #####
############################################################
#function to fill NA values
fill_na_score <- function(data, rows, datapop) {
  for (i in 1:rows) {
    start = FALSE
    end = datapop %>% filter(session == data[i,1])
    end <- ceiling(max(end$time)/gap+1)
    for (j in 2:length(data)){
      if (j <= end){
        if (is.na(data[i,j])){
          if (start){
            data[i,j] <- data[i, j-1]
          }else{
            data[i,j] <- 400
          }
        }else{
          start = TRUE
        }
      }else{
        data[i,j] <- 0
      }
    }
  }
  return(data)
}

###funçao para remover metas que nao houveram plantação
clean_goal <- function(data, goal) {
    remove_lines[1] = 0
    j = 1
    for (i in 1:length(goal$session)){  
        max_time <- filter(data, data$session == goal$session[i])
        max_time <- max(max_time[, 'time'])
        max_time
        if(goal$time[i] >= max_time){
            remove_lines[j] = i
            j = j+1
        }
    }
    for (i in 1:length(remove_lines)){
    }
    copy <- goal[-c(remove_lines), ] 
    return(copy)
}

####função para preencher NA nos dataframe das metas
fill_na_meta_version <- function(data) {
  for (i in 1:nrow(data)) {
    start = FALSE
    for (j in 2:length(data)){
      if (is.na(data[i,j])){
         if (start){
           data[i,j] <- data[i, j-1]
         }else{
           data[i,j] <- 0
         }
       }else{
        start = TRUE
      }
    }
  }
  return(data)
}