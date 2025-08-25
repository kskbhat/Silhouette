## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----check-packages, echo=FALSE, message=FALSE, warning=FALSE-----------------
required_packages <- c("proxy", "ppclust", "blockcluster", "cluster", "factoextra", "ggplot2")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  message("❌ The following required packages are not installed:\n")
  message(paste0("- ", missing_packages, collapse = "\n"), "\n")
  message("📦 To install them, run the following in R:\n")
  message("```r")
  message(sprintf("install.packages(c(%s))", paste(sprintf('\"%s\"', missing_packages), collapse = ", ")))
  message("```")
  
  knitr::knit_exit()
}

## ----setup,echo=FALSE, include=FALSE------------------------------------------
library(Silhouette)
library(proxy)
library(ppclust)
library(cluster)
library(factoextra)
library(blockcluster)
library(ggplot2)
library(drclust)
set.seed(123)

## ----kmeans-------------------------------------------------------------------
data(iris)
km <- kmeans(iris[, -5], centers = 3)

## ----crisp-silhouette1, fig.width=7, fig.height=4, fig.alt = "fig1.1"---------
library(proxy)
dist_matrix <- proxy::dist(iris[, -5], km$centers)
sil <- Silhouette(dist_matrix)
head(sil)
summary(sil)
plot(sil)

## ----crisp-silhouette2, fig.width=7, fig.height=4, fig.alt = "fig1.2"---------
sil_pac <- Silhouette(dist_matrix, method = "pac", sort = TRUE)
head(sil_pac)
summary(sil_pac)
plot(sil_pac)

## ----crisp-silhouette3--------------------------------------------------------
s <- summary(sil_pac,print.summary = TRUE)
# summary table
s$sil.sum
# cluster wise silhouette widths
s$clus.avg.widths
# Overall average silhouette width
s$avg.width

## ----fm-----------------------------------------------------------------------
library(ppclust)
data(iris)
fm <- ppclust::fcm(x = iris[, -5], centers = 3)

## ----crisp-silhouette4, fig.width=7, fig.height=4, fig.alt = "fig1.2"---------
sil_fm <- Silhouette(fm$d)
plot(sil_fm)

## ----crisp-silhouette5, fig.width=7, fig.height=4, fig.alt = "fig1.3"---------
sil_fcm <- Silhouette(prox_matrix = "d", clust_fun = fcm, x = iris[, -5], centers = 3)
plot(sil_fcm)

## ----fuzzy-silhouette4.1, fig.width=7, fig.height=4---------------------------
data(iris)
fm1 <- ppclust::fcm(x = iris[, -5], centers = 3)

## ----fuzzy-silhouette4, fig.width=7, fig.height=4, fig.alt = "fig1.6"---------
sil_fm1 <- Silhouette(prox_matrix = fm1$d, prob_matrix = fm1$u, average = "fuzzy")
plot(sil_fm1)

## ----fuzzy-silhouette5, fig.width=7, fig.height=4, fig.alt = "fig1.3"---------
library(ppclust)
sil_fcm1 <- Silhouette(prox_matrix = "d", prob_matrix = "u", average = "fuzzy", clust_fun = fcm, x = iris[, -5], centers = 3)
plot(sil_fcm1)

## ----fcm----------------------------------------------------------------------
data(iris)

# FCM clustering
fcm_result <- ppclust::fcm(iris[, 1:4], 3)

# FCM2 clustering
fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)

## ----softSilhouette, fig.width=7, fig.height=4, fig.alt = "fig2.1"------------
# Soft silhouette for FCM
sil_fcm <- softSilhouette(prob_matrix = fcm_result$u)
plot(sil_fcm)

# Soft silhouette for FCM2
sil_fcm2 <- softSilhouette(prob_matrix = fcm2_result$u)
plot(sil_fcm2)

## ----softSilhouette1, fig.width=7, fig.height=4, fig.alt = "fig2.2"-----------
sfcm <- summary(sil_fcm, print.summary = FALSE)
sfcm2 <- summary(sil_fcm2, print.summary = FALSE)

cat("FCM average silhouette width:", sfcm$avg.width, "\n",
    "FCM2 average silhouette width:", sfcm2$avg.width)


## ----cer-db-silhouette, fig.width=7, fig.height=4, fig.alt = "fig2.3"---------
# Certainty-based silhouette for FCM and FCM2
  cer_fcm <- cerSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
  plot(cer_fcm)
  
  cer_fcm2 <- cerSilhouette(prob_matrix = fcm2_result$u, print.summary = TRUE)
  plot(cer_fcm2)

# Density-based silhouette for FCM and FCM2
  db_fcm <- dbSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
  plot(db_fcm)
  
  db_fcm2 <- dbSilhouette(prob_matrix = fcm2_result$u, print.summary = TRUE)
  plot(db_fcm2)

## ----cer-db-silhouette-summary------------------------------------------------
# Compare average silhouette widths across all methods
  # Summary for FCM
  cer_sfcm <- summary(cer_fcm, print.summary = FALSE)
  db_sfcm <- summary(db_fcm, print.summary = FALSE)
  
  # Summary for FCM2
  cer_sfcm2 <- summary(cer_fcm2, print.summary = FALSE)
  db_sfcm2 <- summary(db_fcm2, print.summary = FALSE)
  
  # Print comparison
  cat("FCM - Soft silhouette:", sfcm$avg.width, "\n",
      "FCM - Certainty silhouette:", cer_sfcm$avg.width, "\n",
      "FCM - Density-based silhouette:", db_sfcm$avg.width, 
      "\n\n","FCM2 - Soft silhouette:", sfcm2$avg.width, 
      "\n","FCM2 - Certainty silhouette:", cer_sfcm2$avg.width, 
      "\n","FCM2 - Density-based silhouette:", db_sfcm2$avg.width, "\n")

## ----screeplot1---------------------------------------------------------------
data(iris)
avg_sil_width <- rep(NA,7)
for (k in 2:7) {
  sil_out <- Silhouette(
    prox_matrix = "d",
    method = "pac",
    clust_fun = ppclust::fcm,
    x = iris[, 1:4],
    centers = k)
  avg_sil_width[k] <- summary(sil_out, print.summary = FALSE)$avg.width
}

## ----screeplot2, fig.width=7, fig.height=4, fig.alt = "fig3.1"----------------
plot(avg_sil_width,
  type = "o",
  ylab = "Overall Silhouette Width",
  xlab = "Number of Clusters",
  main = "Silhouette Scree Plot"
)

## ----plot0, fig.width=6, fig.height=4, fig.alt = "fig4.0"---------------------
data(iris)
  km_out <- kmeans(iris[, -5], 3)
  dist_mat <- proxy::dist(iris[, -5], km_out$centers)
  sil_obj <- Silhouette(dist_mat)
  plot(sil_obj)                   # S3 method auto-dispatch
  plotSilhouette(sil_obj)         # explicit call (identical output)

## ----plot1, fig.width=6, fig.height=4, fig.alt = "fig4.1"---------------------
library(cluster)
pam_result <- pam(iris[, 1:4], k = 3)
plotSilhouette(pam_result) # for cluster::pam object

clara_result <- clara(iris[, 1:4], k = 3)
plotSilhouette(clara_result)

fanny_result <- fanny(iris[, 1:4], k = 3)
plotSilhouette(fanny_result)

## ----plot2, fig.width=6, fig.height=4, fig.alt = "fig4.2"---------------------
sil_base <- cluster::silhouette(pam_result)
plotSilhouette(sil_base)

## ----plot3, fig.width=6, fig.height=4, fig.alt = "fig4.3"---------------------
library(factoextra)
eclust_result <- eclust(iris[, 1:4], "kmeans", k = 3, graph = FALSE)
plotSilhouette(eclust_result)

hcut_result <- hcut(iris[, 1:4], k = 3)
plotSilhouette(hcut_result)

## ----plot3.1, fig.width=7, fig.height=6, fig.alt = "fig4.3.1"-----------------
library(drclust)
# Loading the numeric in matrix 
iris_mat <- as.matrix(iris[,-5])
#applying a clustering algorithm
drclust_out <- dpcakm(iris_mat, 20, 3)
#silhouette based on the data and the output of the clustering algorithm
d <- silhouette(iris_mat, drclust_out)
plotSilhouette(d$cl.silhouette)

## ----plot4, fig.width=6, fig.height=4, fig.alt = "fig4.4"---------------------
data(iris)
fcm_out <- ppclust::fcm(iris[, 1:4], 3)
sil_fuzzy <- Silhouette(
  prox_matrix = "d", prob_matrix = "u", clust_fun = fcm,
  x = iris[, 1:4], centers = 3, sort = TRUE
)
plot(sil_fuzzy, summary.legend = FALSE, grayscale = TRUE)

## ----plot5, fig.width=6, fig.height=4, fig.alt = "fig4.5"---------------------
plotSilhouette(sil_fuzzy, grayscale = TRUE) # Use grayscale palette
plotSilhouette(sil_fuzzy, summary.legend = TRUE) # Include size + avg silhouette in legend
plotSilhouette(sil_fuzzy, label = TRUE) # Label bars with row index

## ----custom1, fig.width=7, fig.height=4, fig.alt = "fig5.1"-------------------
# Create a custom Silhouette object
cluster_assignments <- c(1, 1, 2, 2, 3, 3)
neighbor_clusters <- c(2, 2, 1, 1, 1, 1)
silhouette_widths <- c(0.8, 0.7, 0.6, 0.9, 0.5, 0.4)
weights <- c(0.9, 0.8, 0.7, 0.95, 0.6, 0.5)

sil_custom <- getSilhouette(
  cluster = cluster_assignments,
  neighbor = neighbor_clusters,
  sil_width = silhouette_widths,
  weight = weights,
  proximity_type = "similarity",
  method = "pac",
  average = "fuzzy"
)
# Validate the object
is.Silhouette(sil_custom)               # Basic class check: TRUE
is.Silhouette(sil_custom, strict = TRUE) # Strict structural validation: TRUE
is.Silhouette(data.frame(a = 1:6))      # Non-Silhouette object: FALSE
# Visualize the custom Silhouette object
plotSilhouette(sil_custom, summary.legend = TRUE)

## ----ext1---------------------------------------------------------------------
library(blockcluster)
data(iris)
result <- coclusterContinuous(as.matrix(iris[, -5]), nbcocluster = c(3, 2))

## ----ext2---------------------------------------------------------------------
sil_mode1 <- softSilhouette(
  prob_matrix = result@rowposteriorprob,
  method = "pac",
  print.summary = FALSE
)
sil_mode2 <- softSilhouette(
  prob_matrix = result@colposteriorprob,
  method = "pac",
  print.summary = FALSE
)

## ----ext3---------------------------------------------------------------------
ext_sil <- extSilhouette(
  sil_list = list(sil_mode1, sil_mode2),
  dim_names = c("Rows", "Columns"),
  print.summary = TRUE
)

