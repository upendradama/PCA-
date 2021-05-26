---
output:
  html_notebook: default
  html_document: default
---
# PCA & clustering

### Problem Statement:- 

  - Perform Principal component analysis and perform clustering using first 
3 principal component scores (both heirarchial and k mean clustering(scree plot or elbow curve) and obtain optimum number of clusters and check whether we have obtained same number of clusters with the original data (class column we have ignored at the begining who shows it has 3 clusters)
  
### Data Understanding and Preparation

```{r}
#Reading the dataset
library(readxl)
univ <- read_excel("~/desktop/Digi 360/Module 14/University_Clustering.xlsx")
```

```{r}
head(univ)
```

```{r}
any(is.na(univ))
```


```{r}
#Removing First column since it is categorical
univ_num <- univ[, -c(1,2)]
head(univ_num)
```


```{r}
summary(univ_num)
```

### Scaling

Before applying PCA or Clustering, we have to normalize the data so that the scale of each variable is the same. Why is this important? Well, if the scale of the variables is not the same, the model might become biased towards the variables with a higher magnitude.

```{r}
univ_scaled <- as.data.frame(scale(univ_num))
summary(univ_scaled)
```

Since all the values here are continuous numerical values, you will use the euclidean distance method.

```{r}
nrow(univ_scaled)
```

### Cluster Tendency with Hopkins Statistic

```{r}
library(factoextra)
library(ggplot2)
# Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(univ_scaled, n = 24, graph = FALSE)
res$hopkins_stat
```

Since hopkins is above the threshold, our data is tendency to clustering.

### Elbow Curve

```{r}
wss = (nrow(univ_scaled)-1)*sum(apply(univ_scaled, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:8) wss[i] = sum(kmeans(univ_scaled, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
```

### Building the Model

```{r}
model <- kmeans(univ_scaled, 3) # 3 cluster solution
str(model)
```

```{r}
fviz_cluster(model, data = univ_scaled)
```

### Applying PCA

```{r}
pcaObj<-princomp(univ_scaled, cor = TRUE, scores = TRUE, covmat = NULL)
```

```{r}
summary(pcaObj)
```

```{r}
loadings(pcaObj)
```

```{r}
plot(pcaObj) # graph showing importance of principal components 
```

```{r}
biplot(pcaObj)
```

```{r}
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
```

```{r}
pcaObj$scores
pcaObj$scores[,1:3]
```

```{r}
# Top 3 pca scores 
final<-cbind(univ[,1:2],pcaObj$scores[,1:3])
View(final)
```

### K- Means Clustering

```{r}
model <- kmeans(pcaObj$scores[,1:3], 3) # 3 cluster solution
str(model)
```

```{r}
fviz_cluster(model, data = pcaObj$scores[,1:3])
```


```{r}
#Appending the clusters to original dataframe.
suppressPackageStartupMessages(library(dplyr))
univ_cl <- mutate(univ, model$cluster)
```

```{r}
head(univ_cl)
```

### Visualizing few attributes

```{r}
suppressPackageStartupMessages(library(ggplot2))
ggplot(univ_cl, aes(x=State, y = Expenses, color = factor(model$cluster))) + geom_point()
```

```{r}
univ_num <- univ_num %>%
  mutate(Cluster = model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
```

```{r}
head(univ_num)
```


```{r}
#Writing the final file with assigned cluster IDs.
write.csv(univ_cl, file="~/desktop/Digi 360/Module 14/University_final_R.csv")
```

### Hierarchical clustering

```{r}
dist_mat <- dist(pcaObj$scores[,1:3], method = 'euclidean')
hclust_ward <- hclust(dist_mat, method = 'ward.D2')
```


```{r}
#Plotting the dendrogram
plot(hclust_ward)
```

```{r}
#cutting the dendrogram
cut_ward <- cutree(hclust_ward, k = 3)
```

```{r}
plot(hclust_ward)
rect.hclust(hclust_ward , k = 3, border = 2:6)
abline(h = 5.3, col = 'red')
```

Let's visualize the tree with different colored branches using package `dendextend`

```{r}
suppressPackageStartupMessages(library(dendextend))
ward_dend_obj <- as.dendrogram(hclust_ward)
ward_col_dend <- color_branches(ward_dend_obj, h = 5.3)
plot(ward_col_dend)
```

```{r}
#Appending the clusters to original dataframe.
suppressPackageStartupMessages(library(dplyr))
univ_cl <- mutate(univ, cluster = cut_ward)
```

```{r}
#Writing the final file with assigned cluster IDs.
write.csv(univ_cl, file="~/desktop/Digi 360/Module 14/univ_final_R.csv")
```

### Conclusion

- With PCA, we could see that same number of clusters formed that are formed without PCA. 

