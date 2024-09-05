library(here)
library(terra)
library(tidyverse)
library(tidyterra)

unit_vect <- function(p1,p2){
  # Define points P1 and P2
  P1 <- c(geom(p1)[1,3],geom(p1)[1,4])
  P2 <- c(geom(p2)[1,3],geom(p2)[1,4])
  
  # Calculate the direction vector
  v <- P2 - P1
  
  # Normalize the vector (optional)
  magnitude <- sqrt(sum(v^2))
  unit_vector <- v / magnitude
  
  # Output the direction vector and the unit vector
  return(unit_vector)
}
avg_flow_gpt <- function(x){
  flow <- terrain(x, v="flowdir")
  # Map flow direction values to angles with the 90-degree correction
  flow_angles <- classify(flow, 
                          rbind(c(1, pi/2),      # East (1) → π/2 radians
                                c(2, 3*pi/4),    # SE (2) → 3π/4 radians
                                c(4, pi),        # South (4) → π radians
                                c(8, 5*pi/4),    # SW (8) → 5π/4 radians
                                c(16, 3*pi/2),   # West (16) → 3π/2 radians
                                c(32, 7*pi/4),   # NW (32) → 7π/4 radians
                                c(64, 0),        # North (64) → 0 radians (or 2π radians)
                                c(128, pi/4)))   # NE (128) → π/4 radians
  # Calculate the x and y components of the flow direction
  x_component <- cos(flow_angles)
  y_component <- sin(flow_angles)
  # Compute the global mean x and y components
  mean_x <- global(x_component, mean, na.rm = TRUE)
  mean_y <- global(y_component, mean, na.rm = TRUE)
  
  # Calculate the average flow direction in radians
  avg_direction_radians <- atan2(mean_y[1,1], mean_x[1,1])
  
  # Convert the average direction to degrees (optional for interpretation)
  avg_direction_degrees <- (avg_direction_radians * 180) / pi
  
  # Ensure the result is in the range 0-360 degrees
  if (avg_direction_degrees < 0) {
    avg_direction_degrees <- avg_direction_degrees + 360
  }

  return(avg_direction_degrees)
}
avg_flow_njt <- function(min_elev, max_elev, plt_cntr){
  v1 <- unit_vect(min_elev, plt_cntr)
  v2 <- unit_vect(plt_cntr, max_elev)
  # Calculate the average vector
  v_avg <- (v1 + v2) / 2
  
  # Calculate the angle in radians using atan2
  angle_radians <- atan2(v_avg[2], v_avg[1])
  
  # Convert to degrees
  angle_degrees <- angle_radians * (180 / pi)
  
  # Output the angle in degrees
  return(angle_degrees)
}
plot_flow <- function(dir, dom, color){
  # Convert degrees to radians (arrows() expects radians)
  angle_radians <- dir * pi / 180
  
  # Define the center of the plot
  x_center <- geom(centroids(dom))[1,3]
  y_center <- geom(centroids(dom))[1,4]
  
  # Define the length of the vector
  length <- 200
  
  # Compute the end coordinates of the vector using trigonometry
  x_end <- x_center + length * cos(angle_radians)
  y_end <- y_center + length * sin(angle_radians)
  
  # Draw the vector (arrow) from the center to the calculated end point
  arrows(x_center, y_center, x_end, y_end, lwd = 2, col=color)
}
basin_topo <- function(fire, dem, basins, number){
  basin <- basins[num,]
  domain <- buffer(vect(ext(basin), crs=crs(basin)), 50, joinstyle="mitre")
  # domain <- vect(here(fire,"Sample_Basins",paste0(substr(fire,1,3),num),paste0(substr(fire,1,3),num,".shp")))
  
  topo <- crop(dem, domain)
  names(topo) <- "elev"
  
  min_elev <- as.points(topo) %>% filter(elev == min(elev))
  max_elev <- as.points(topo) %>% filter(elev == max(elev))
  plt_cntr <- centroids(domain)
  # flow_gpt <- avg_flow_gpt(topo)
  flow_njt <- avg_flow_njt(min_elev,max_elev,plt_cntr)
  
  plot(topo)
  plot(basin, add=T)
  plot(min_elev, add=T)
  plot(max_elev, add=T)
  plot(plt_cntr, add=T)
  # plot_flow(flow_gpt, domain, "red")
  plot_flow(flow_njt, domain, "blue")
}

fire <- "KNP"
dem <- rast(here(fire,paste0(fire,"_DEM.tif")))
basins <- vect(here(fire,paste0(fire,"_sample_basins.shp")))

num <- 3
basin_topo(fire, dem, basins, num)










