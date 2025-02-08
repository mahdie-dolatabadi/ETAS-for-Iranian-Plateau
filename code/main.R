## Libraries are imported
print("importing library")
library("ETAS")
# source("catalog.R")

## Reading text; convert to dataframe
print("making frame...")
zagros <- read.table("Zagros90-24/zagros90-24.xls.txt", header = TRUE)
coordinates <- read.table("polygon mirzai/polygon limit/zagros.txt", header = FALSE)



# head(coordinates)
# for plotting coordinates in websites
# https://www.keene.edu/campus/maps/tool/
# use the reversed coordinates
reversed_coordinates <- coordinates[, ncol(coordinates):1]
# Save the modified dataframe as a text file
write.table(reversed_coordinates, "Zagros90-24/reversed_coordinates.txt", sep = ",", quote = FALSE, row.names = FALSE)




zagpoly <- list(lat = coordinates[, 1], long = coordinates[, 2])

# Assuming zagpoly is your polygon
region.poly_reversed <- zagpoly
region.poly_reversed$long <- rev(zagpoly$long)
region.poly_reversed$lat <- rev(zagpoly$lat)

## for checking if the order of coordinates are correct or not
# library(spatstat.geom)
# window <- spatstat.geom::owin(poly = list(x = region.poly_reversed$long, y = region.poly_reversed$lat))


# making zagros catalog
zagros.cat <- catalog(zagros, study.start="1994/01/01", study.end="2024/01/01",  region.poly=region.poly_reversed, mag.threshold=4.1)
print("zagros catalog is made.")
# print(zagros.cat)
# # plotting catalog
# plot(zagros.cat)
# print("zagros is plotted.")

### use below ###
#   mbk <- seq(0, ceiling(10 * max(x$revents[, 4])) / 10 + 0.1 , 0.1) + x$mag.threshold

# fit the model
print("fitting the model ...")
nthreads <- parallel::detectCores()
zagros.fit <- etas(zagros.cat, nthreads=nthreads)

# zagros.fit
print("zagros.fit")
zagros.fit
saveRDS(zagros.fit, file = "zagros.rds")
# plotting zagros.fit
print("plotting...")
plot(zagros.fit)

### rates
# Declustering Probabilities, Background Seismicity 
# Rate and Clustering Coefficient
# Functions to estimate the declustering probabilities, 
# background seismicity rate and clustering (triggering)
#  coefficient for a fitted ETAS model.
print("rate...")
rates(zagros.fit)

# estimating the declustering probabilities
pr <- probs(zagros.fit)
print(pr)

# summary of prob pobability
summary(pr$prob)
zagros.res <- resid.etas(zagros.fit)
summary(zagros.res$tres)
summary(na.omit(zagros.res$sres$z))
ks.test(zagros.res$U, punif)