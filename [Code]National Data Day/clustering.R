library(cluster)
library(factoextra)
library(cluster.datasets)
library(xlsx)
library(ggbiplot)



#Import dataset
df <- scale(T)
boxplot(Testone$`Labu Siam`, main ="Boxplot variabel Labu Siam")
summary(Testone$`Labu Siam`)
boxplot(df, main = "Boxplot variabel dataset sesudah scaling")
#Hopkins Statistics
res <- get_clust_tendency(df, n = nrow(df) - 1, graph = FALSE)
res$hopkins_stat
rownames(df) <- c("Kab. Banjarnegara","Kab. Banyumas","Kab. Batang","Kab. Blora","Kab. Boyolali","Kab. Brebes","Kab. Cilacap","Kab. Demak","Kab. Grobogan","Kab. Jepara","Kab. Karanganyar","Kab. Kebumen","Kab. Kendal","Kab. Klaten","Kab. Kudus","Kab. Magelang","Kab. Pati","Kab. Pekalongan","Kab. Pemalang","Kab. Purbalingga","Kab. Purworejo","Kab. Rembang","Kab. Semarang","Kab. Sragen","Kab. Sukoharjo","Kab. Tegal","Kab. Temanggung","Kab. Wonogiri","Kab. Wonosobo","Kota Magelang","Kota Pekalongan","Kota Salatiga","Kota Semarang","Kota Surakarta","Kota Tegal")

#Dissimilarity Matrix
df.eucl <- dist(df, method = "euclidean")
fviz_dist(df.eucl)
round(as.matrix(df.eucl)[1:3, 1:3], 1)

pr.pca <- prcomp(df, center = TRUE)
ggplot(pr.pca)

#tentuin jumlah k optimal
fviz_nbclust(df, pam, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
#Proses Clustering
pam.res = pam(df, 4, metric = "euclidean")
pam.res$medoids # Medoid yang digunakan
pam.res$silinfo
plot(pam.res$silinfo)
print(pam.res)
gg <- fviz_cluster(pam.res, 
             palette = c("#FF0000", "#3e0e35","#45de88","#0077ff","#00FFFF"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()

             )
gg
str(si<-silhouette(pam.res))
plot(si, col = c("#FF0000", "#3e0e35","#45de88","#0077ff","#00FFFF"), max.strlen= 9, main = "Plot Silhouette untuk evaluasi cluster", xlab = "Panjang Silhouette", do.clus.stat=TRUE)
ssi<-summary(si)
ssi
library(corrplot)
res <- cor(df)
round(res, 2)
corrplot(res, tl.col = "black", tl.srt = 45, method="circle")
 boxplot(res)

