library(factoextra)

# Loading the Data

heptathlon <- read.csv("heptathlon.csv")
head(heptathlon, 5)

heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

heptathlon_orig <- heptathlon

heptathlon <- heptathlon %>% 
  select(-score)
# Principal Component Analysis

pca <- prcomp(heptathlon, scale = TRUE)
pca

# PCA summaries

summary(pca)

get_eig(pca)

colSums(get_eig(pca))

str(pca)

pca$x

pca$rotation



plot(pca)


biplot(pca, xpd = TRUE)

fviz_pca_var(pca)

plot(rowSums(heptathlon), pca$x[,1])

# Cosine² values

var <- get_pca_var(pca)
var

fviz_cos2(pca, choice = "var", axes = 1:2) # High cos2 value means important variables, but


fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # repel is for text overlapping, but not necessary here


model_lm <- lm(score ~ hurdles + highjump, data = heptathlon_orig)
summary(model_lm)

heptathlon_orig$ypred_lm <- predict(model_lm)
sqrt(mean(model_lm$residuals^2))


x.new <- cbind(heptathlon, pca$x)
str(x.new)
pca_model <- lm(score ~ PC1 + PC2,data = x.new )
summary(pca_model)
