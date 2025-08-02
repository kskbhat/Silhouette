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
sil_fm1 <- Silhouette(prox_matrix = fm1$d, prob_matrix = fm1$u)
plot(sil_fm1)

## ----fuzzy-silhouette5, fig.width=7, fig.height=4, fig.alt = "fig1.3"---------
sil_fcm1 <- Silhouette(prox_matrix = "d", prob_matrix = "u", clust_fun = fcm, x = iris[, -5], centers = 3)
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

cat("FCM average silhouette width:", sfcm$avg.width, "\n")
cat("FCM2 average silhouette width:", sfcm2$avg.width, "\n")

## ----screeplot1---------------------------------------------------------------
data(iris)
avg_sil_width <- numeric(6)
for (k in 2:7) {
  sil_out <- Silhouette(
    prox_matrix = "d",
    proximity_type = "dissimilarity",
    prob_matrix = "u",
    clust_fun = ppclust::fcm,
    x = iris[, 1:4],
    centers = k,
    print.summary = FALSE,
    sort = TRUE
  )
  avg_sil_width[k - 1] <- summary(sil_out, print.summary = FALSE)$avg.width
}

## ----screeplot2, fig.width=7, fig.height=4, fig.alt = "fig3.1"----------------
plot(avg_sil_width,
  type = "o",
  ylab = "Overall Silhouette Width",
  xlab = "Number of Clusters",
  main = "Silhouette Scree Plot"
)

## ----plot0, fig.width=7, fig.height=4, fig.alt = "fig4.0"---------------------
data(iris)
  km_out <- kmeans(iris[, -5], 3)
  dist_mat <- proxy::dist(iris[, -5], km_out$centers)
  sil_obj <- Silhouette(dist_mat)
  plot(sil_obj)                   # S3 method auto-dispatch
  plotSilhouette(sil_obj)         # explicit call (identical output)

## ----plot1, fig.width=7, fig.height=4, fig.alt = "fig4.1"---------------------
library(cluster)
pam_result <- pam(iris[, 1:4], k = 3)
plotSilhouette(pam_result) # for cluster::pam object

clara_result <- clara(iris[, 1:4], k = 3)
plotSilhouette(clara_result)

fanny_result <- fanny(iris[, 1:4], k = 3)
plotSilhouette(fanny_result)

## ----plot2, fig.width=7, fig.height=4, fig.alt = "fig4.2"---------------------
sil_base <- silhouette(pam_result)
plotSilhouette(sil_base)

## ----plot3, fig.width=7, fig.height=4, fig.alt = "fig4.3"---------------------
library(factoextra)
eclust_result <- eclust(iris[, 1:4], "kmeans", k = 3, graph = FALSE)
plotSilhouette(eclust_result)

hcut_result <- hcut(iris[, 1:4], k = 3)
plotSilhouette(hcut_result)

## ----plot4, fig.width=7, fig.height=4, fig.alt = "fig4.4"---------------------
data(iris)
fcm_out <- ppclust::fcm(iris[, 1:4], 3)
sil_fuzzy <- Silhouette(
  prox_matrix = "d", prob_matrix = "u", clust_fun = fcm,
  x = iris[, 1:4], centers = 3, sort = TRUE
)
plot(sil_fuzzy, summary.legend = FALSE, grayscale = TRUE)

## ----plot5, fig.width=7, fig.height=4, fig.alt = "fig4.5"---------------------
plotSilhouette(sil_fuzzy, grayscale = TRUE) # Use grayscale palette
plotSilhouette(sil_fuzzy, summary.legend = TRUE) # Include size + avg silhouette in legend
plotSilhouette(sil_fuzzy, label = TRUE) # Label bars with row index

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

