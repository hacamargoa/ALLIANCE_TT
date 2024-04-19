
#Principal components 
Rice_pca <- Rice_data[, -c(1:3,28)]
pca_result <- prcomp(Rice_pca, scale. = TRUE)
Com_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)

#plot of accumulated variance
ggplot(data = data.frame(acum=cumsum(Com_var), pc = 1:24),
       aes(x = pc, y = acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Principal Component",
       y = "Accum variance")
ncomp<-sum(cumsum(Com_var)<0.80) # components with 80% variation
Composition<-pca_result$rotation[,1:ncomp]
#PC1 vs PC2
plot(Composition[,1], Composition[,2], xlab = "PC1", ylab = "PC2",col = "white")
text(Composition[, 1], Composition[, 2], labels = rownames(Composition), pos = 3, offset = 0.5, col = "blue")
arrows(0, 0, pca_result$rotation[, 1], pca_result$rotation[, 2], length = 0.1, col = "red")
#PC3 vs PC4
plot(Composition[,3], Composition[,4], xlab = "PC3", ylab = "PC4",col = "white")
text(Composition[, 3], Composition[, 4], labels = rownames(Composition), pos = 1, offset = 0.5, col = "blue")
arrows(0, 0, pca_result$rotation[, 3], pca_result$rotation[, 4], length = 0.1, col = "red")

#Multiple regresion Yield ~ P. Components
pcas <- pca_result$x[, 1:3]
Rice_new<-as.data.frame(cbind(pcas,Yield=Rice_data$Yield))
plots2 <- list()
a=1
for (i in names(Rice_new[,-5])) {
    plots2[[a]] <- ggplot(Rice_new) +
    geom_point(aes(x = !!sym(colnames(Rice_new)[a]), y = Yield)) +
    labs(x = colnames(Rice_new)[a], y = "Yield")
  a=a+1
}
panel2 <- cowplot::plot_grid(plotlist = plots2, nrow = 2, ncol = 2)
panel2

Rice_model <- lm(Yield ~ ., data = Rice_new)
summary(Rice_model)

