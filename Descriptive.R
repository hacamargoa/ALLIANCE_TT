install.packages("readxl")
#install.packages("car")
library(readxl)
library(ggplot2)

#descriptive stats
Rice_data<-as.data.frame(read_excel("data_base_RF.xlsx"))
names(Rice_data) <- gsub("\\s.*", "",names(Rice_data))
summary(Rice_data)
cor_matrix<-cor(Rice_data[c(4:28)])

stat_df<-Rice_data[-c(1,2,3)]
average <- sapply(stat_df, mean)
sd <- sapply(stat_df, sd)
maximum <- sapply(stat_df, max)
minimum <- sapply(stat_df, min)

# Combine the results into a dataframe
stats_df <- data.frame(
  Average = average,
  SD = sd,
  Maximum = maximum,
  Minimum = minimum
)
stats_df$CV<-stats_df$SD/stats_df$Average
clipr::write_clip(stats_df) #copy dataframe

# Loop to create scatter plots and cor t test
plots <- list()
cor_df<-data.frame()
a=1
  for (i in names(Rice_data[-c(1:3)])) {
    if (i != "Yield" && is.numeric(Rice_data[[i]])) {
      temp <- cor.test(Rice_data[[i]], Rice_data$Yield)
  cor_df[i,1]<-temp$estimate
  cor_df[i,2]<-temp$p.value
      }
    plots[[a]] <- ggplot(Rice_data) +
    geom_point(aes(x = !!sym(colnames(Rice_data)[a+3]), y = Yield)) +
    labs(x = colnames(Rice_data)[a+3], y = "Yield")
  a=a+1
    }
clipr::write_clip(cor_df)

# Combine the plots into a panel
panel <- cowplot::plot_grid(plotlist = plots, nrow = 6, ncol = 4)
panel

