library("ETAS")

data <- readRDS("zagros.rds")

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
# print(ceiling(2.4*10))
zagros.cat <- catalog(zagros, study.start="1994/01/01", study.end="2024/01/01",  region.poly=region.poly_reversed, mag.threshold=4.1)
print("zagros catalog is made.")

# estimating the declustering probabilities
pr <- probs(data)

# Get the range of longitude and latitude coordinates
coords <- zagros.cat$longlat.coord[pr$target, 1:2]
lon_range <- range(coords[, 1])
lat_range <- range(coords[, 2])

# Save the plot to a file
png("geographical_data_points.png", width = 800, height = 600)

# Plot the data points with dynamic map dimensions
plot(coords[pr$prob <= 0.05, ], 
     pch = 1, col = "#000000", cex = 2, lwd = 1.2,
     xlab = "Longitude", 
     ylab = "Latitude", cex.main = 1.5, cex.lab = 1.2, 
     xlim = lon_range, ylim = lat_range) # Set dynamic limits

points(coords[pr$prob > 0.95, ], 
       pch = 8, col = "#D50032", cex = 2, lwd = 1)

# Add the map
map("world", add = TRUE, col = "lightblue")

legend("topright", c("Background", "Triggered"), 
       pch = c(1, 8), col = c("#000000", "#D50032"), 
       cex = 1.3, title = "Categories", title.col = "darkblue", text.col = "black",
       box.col = "darkblue", box.lwd = 2, bg = "white", inset = 0.02)

# Close the graphical device
dev.off()
