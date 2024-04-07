library(ggplot2)
library(tidyverse)
library(dplyr)
library(cluster)
library(factoextra)
library(reshape2)
library(gridExtra)
library(corrplot)
library(RColorBrewer)
library(dbscan)
library(Rtsne)



########################### DATA CLEANING ######################################


data.original <- read_csv("C:/Users/sangi/Desktop/SL exam/Basketball/players_stats_by_season_full_details.csv")

selected_leagues <- c("Italian-Lega-Basket-Serie-A", "Spanish-ACB", "German-BBL", 
                      "Turkish-BSL", "French-Jeep-Elite")

data.original$First_nationality <- trimws(sub("\\/.*", "", data.original$nationality))

data.absolute <- data.original %>%
  mutate(Age = 2020 - birth_year, Nationality = nationality, Height_cm = height_cm, 
         Weight_kg = weight_kg) %>%
  dplyr::select(League, Player, Nationality, First_nationality, Age, Height_cm, Weight_kg, GP:PTS) %>%
  filter(data.original$Season == "2018 - 2019" & data.original$League %in% selected_leagues) %>%
  arrange(desc(PTS)) %>%
  drop_na()

data.per.game <- data.absolute %>%
  mutate(across(c("FGM", "FGA", "3PM", "3PA", "FTM", "FTA", "TOV", "PF", "ORB", 
                  "DRB", "REB", "AST", "STL", "BLK", "PTS"), ~ round( ./GP, 2))) %>%
  arrange(desc(PTS))

new_columns <- c("League", "Player", "Nationality", "First_nationality", 
                 "Age", "Height", "Weight", "Games_played", "Minutes", "FG_made", 
                 "FG_attempts", "3P_made", "3P_attempts", "FT_made", "FT_attempts",
                 "Turnovers", "Fouls", "Off_rebounds", "Def_rebounds",
                 "Tot_rebounds", "Assists", "Steals", "Blocks", "Points")
colnames(data.absolute) <- new_columns
colnames(data.per.game) <- new_columns


view(data.absolute)
view(data.per.game)

numbers.small <- data.absolute %>%
  dplyr::select(FG_made:Points)
numbers.full <- data.absolute %>%
  dplyr::select(Age:Points)



############################## EXPLORING THE DATA ##############################


ggplot(gather(numbers.small), aes(x = key, y = value)) +
  geom_boxplot(color = "sienna4", fill = "darkorange3", alpha = 0.6, outlier.colour = "sienna4") +
  labs(x = "", y = "", title = "Boxplot of player stats\n")

correlated_data <- cor(numbers.full)
corrplot(correlated_data, type = "upper")

num.nations <- n_distinct(data.absolute$First_nationality)
num.double.nation <- sum(str_count(data.absolute$Nationality, " / "))
nations <- unique(data.absolute$First_nationality)

top_nationalities <- names(sort(table(data.absolute$First_nationality), decreasing = TRUE))[1:5]
ggplot(data.absolute[data.absolute$First_nationality %in% top_nationalities, ], 
       aes(x = factor(First_nationality, levels = top_nationalities))) + 
  geom_bar(fill = "chartreuse4", alpha = 0.8) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(
    x = "\nCountry", y = "Number of players\n", title = "Most represented nationalities\n")

usa.it <- data.absolute %>%
  filter(str_detect(Nationality, "United States") & League == "Italian-Lega-Basket-Serie-A")
usa.fr <- data.absolute %>%
  filter(str_detect(Nationality, "United States") & League == "French-Jeep-Elite")
usa.de <- data.absolute %>%
  filter(str_detect(Nationality, "United States") & League == "German-BBL")
usa.es <- data.absolute %>%
  filter(str_detect(Nationality, "United States") & League == "Spanish-ACB")
usa.tr <- data.absolute %>%
  filter(str_detect(Nationality, "United States") & League == "Turkish-BSL")
perc.usa.it <- round(nrow(usa.it) / nrow(data.absolute) * 100, 2)
perc.usa.fr <- round(nrow(usa.fr) / nrow(data.absolute) * 100, 2)
perc.usa.de <- round(nrow(usa.de) / nrow(data.absolute) * 100, 2)
perc.usa.es <- round(nrow(usa.es) / nrow(data.absolute) * 100, 2)
perc.usa.tr <- round(nrow(usa.tr) / nrow(data.absolute) * 100, 2)

italians.it <- data.absolute %>%
  filter(str_detect(Nationality, "Italy") & League == "Italian-Lega-Basket-Serie-A")
french.fr <- data.absolute %>%
  filter(str_detect(Nationality, "France") & League == "French-Jeep-Elite")
germans.de <- data.absolute %>%
  filter(str_detect(Nationality, "Germany") & League == "German-BBL")
spanish.es <- data.absolute %>%
  filter(str_detect(Nationality, "Spain") & League == "Spanish-ACB")
turkish.tr <- data.absolute %>%
  filter(str_detect(Nationality, "Turkey") & League == "Turkish-BSL")
perc.italians.it <- round(nrow(italians.it) / nrow(data.absolute) * 100, 2)
perc.french.fr <- round(nrow(french.fr) / nrow(data.absolute) * 100, 2)
perc.germans.de <- round(nrow(germans.de) / nrow(data.absolute) * 100, 2)
perc.spanish.es <- round(nrow(spanish.es) / nrow(data.absolute) * 100, 2)
perc.turkish.tr <- round(nrow(turkish.tr) / nrow(data.absolute) * 100, 2)

tot.perc.american <- c(perc.usa.it, perc.usa.fr, perc.usa.de, perc.usa.es, perc.usa.tr)
tot.perc.local <- c(perc.italians.it, perc.french.fr, perc.germans.de, perc.spanish.es, perc.turkish.tr)

perc.data <- c(perc.usa.it, perc.usa.fr, perc.usa.de, perc.usa.es, perc.usa.tr,
               perc.italians.it, perc.french.fr, perc.germans.de, perc.spanish.es,
               perc.turkish.tr)
perc.df <- data.frame(selected_leagues, tot.perc.american, tot.perc.local)
perc.df.melted <- melt(perc.df, id.vars = "selected_leagues")

ggplot(data = perc.df.melted, aes(x = selected_leagues, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.8) + labs(title = "Presence of American players\n", x = "", y = "Percentage\n") +
  scale_fill_manual(labels = c("American Players", "Local Players"), 
                    values = c("tot.perc.american" = "darkred", "tot.perc.local" = "mediumseagreen"))

height_distr <- ggplot(data.absolute, aes(x = Height)) + geom_histogram(binwidth = 3, fill = "deepskyblue4", color = "snow1")
weight_distr <- ggplot(data.absolute, aes(x = Weight)) + geom_histogram(binwidth = 2, fill = "darkolivegreen4", color = "snow1")
age_distr <- ggplot(data.absolute, aes(x = Age)) + geom_histogram(binwidth = 2, fill = "darkred", color = "snow1")
grid.arrange(height_distr, weight_distr, top = "Height and weight distribution\n")
age_distr

first_names <- tail(sort(table(sort(sub("\\ .*", "", data.absolute$Player)))), 5)
barplot(first_names, col = "coral3", main = "Most common first names\n")

data.absolute$isAmerican[data.absolute$Nationality == "United States"] <- "American"
data.absolute$isAmerican[data.absolute$Nationality != "United States"] <- "Not american"



#################### PRINCIPAL COMPONENT ANALYSIS ##############################


mypc <- prcomp(numbers.full, scale = TRUE, center = TRUE)
mypc.var <- mypc$sdev^2
mypc.prop.var.expl <- mypc.var/sum(mypc.var)

summary(mypc)

fviz_screeplot(mypc, addlabels = TRUE, title = "Proportion of variance explained by each component\n",
               xlab = "\nPrincipal component", ylab = "Proportion of variance explained\n")

plot(cumsum(mypc.prop.var.expl), main = "Cumulative proportion of variance explained by each component",
     xlab = "Principal component", ylab = "Cumulative proportion", ylim = c(0,1), type = 'b',
     col = "maroon4", pch = 19)
abline(v = 2, h = 0.685, lty = 2)

pca_contrib1 <- fviz_contrib(mypc, choice = "var", col = FALSE, fill = "khaki4", axes = 1,
                             title = "Contribution of variables to the 1st principal component")
pca_contrib2 <- fviz_contrib(mypc, choice = "var", col = FALSE, fill = "purple4", axes = 2,
                             title = "Contribution of variables to the 2nd principal component")

fviz_pca_biplot(mypc, geom.ind = "point", repel = TRUE, col.var = "chocolate4", 
                title = "Biplot of the PCA", habillage = data.absolute$isAmerican,
                palette = c("blue", "darkgreen"), alpha = 0.5)



############################# CLUSTERING #######################################


fviz_nbclust(numbers.full, kmeans, method = "wss", linecolor = "seagreen4") +
  labs(title = "Optimal number of cluster - WSS method\n") + xlab("\nNumber of clusters") +
  ylab("Total WSS\n")
    
fviz_nbclust(numbers.full, kmeans, method = "silhouette", linecolor = "violetred4") +
  labs(title = "Optimal number of clusters - Silhouette method\n") + xlab("\nNumber of clsuters") +
  ylab("Average silhouette width\n")

km.out <- kmeans(numbers.full, centers = 4, nstart = 100)
km.clusters <- km.out$cluster
fviz_cluster(list(data = numbers.full, cluster = km.clusters)) + 
  labs(title = "Cluster plot with k = 4\n") + theme(legend.position = "none")

distances <- dist(numbers.full)
hc_complete.out <- hclust(distances, method = "complete")
hc_clusters <- cutree(hc_complete.out, k = 2)
hc_complete.plot <- fviz_dend(hc_complete.out, k = 2,
                              show_labels = FALSE, main = "Cluster dendrogram, k = 2")
fviz_cluster(list(data = numbers.full, cluster =  hc_clusters))

set.seed(17)
tsne_result <- Rtsne(scale(numbers.full), dims = 2, perplexity = 40)
embedded_points <- tsne_result$Y
tsne_df <- data.frame(x = embedded_points[,1], y = embedded_points[,2])
ggplot(tsne_df, aes(x = x, y = y)) + geom_point(size = 4, color = "brown") + 
  labs(title = "t-SNE visualization", x = "X1", y = "X2")

my_dbscan <- dbscan((tsne_df), eps = 1.4, minPts = 6)
clusters <- my_dbscan$cluster
noise_points <- tsne_df[my_dbscan$cluster == 0, ]
ggplot() + geom_point(aes(x = tsne_df[, 1], y = tsne_df[, 2], color = as.factor(clusters)), size = 5) + 
  geom_point(data = noise_points, aes(x = noise_points[, 1], y = noise_points[, 2]), color = "gray", size = 5) +
  scale_color_discrete(name = "Cluster") + labs(x = "X1", y = "X2", title = "DBSCAN Clustering on t-SNE reduced data") +
  theme_minimal()
