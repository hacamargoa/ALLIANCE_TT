install.packages("readxl")
#install.packages("car")
library(readxl)
library(ggplot2)

Rice_data<-as.data.frame(read_excel("data_base_RF.xlsx"))
names(Rice_data) <- gsub("\\s.*", "",names(Rice_data))
summary(Rice_data)
par(mfrow=c(6, 4))
for (i in 4:27){
  ggplot(Rice_data) +
    geom_point(aes(x = get(colnames(Rice_data[i])), y=Yield))
}
cor_matrix<-cor(Rice_data[c(4:28)])[,25]
summary(Rice_data[i])
plot(Rice_data[4],Rice_data[28])
?plot

plot_list <- list()

# Loop through each column (from 4 to 27) and create scatter plots
for (i in 4:27) {
  plot_list[[i]] <- ggplot(Rice_data) +
    geom_point(aes(x = !!sym(colnames(Rice_data)[i]), y = Yield)) +
    labs(x = colnames(Rice_data)[i], y = "Yield") +
    ggtitle(paste("Scatter plot of", colnames(Rice_data)[i], "vs Yield"))
}

# Combine the plots into a panel
panel_plot <- cowplot::plot_grid(plotlist = plot_list, nrow = 6, ncol = 4)

# Display the panel
panel_plot
