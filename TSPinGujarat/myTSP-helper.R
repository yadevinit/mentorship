cLatLong <- c("lat","long"); cLongLat <- rev(cLatLong)
methods <- union(
  c("identity", "random", "nearest_insertion",
    "cheapest_insertion", "farthest_insertion", "arbitrary_insertion",
    "nn", "repetitive_nn", "two_opt"),
  c("concorde", "linkern") # external Concorde solvers.
)
mySpots <- function(df1, longLat=cLongLat){
  # ref https://stackoverflow.com/questions/7309121/preferred-order-of-writing-latitude-longitude-tuples-in-gis-services:
#  pts <- as.matrix(df1[, longLat])
#  (mp1 = st_multipoint(pts))
#  # sf_use_s2(use_s2=TRUE); s2_distance_matrix(mp1)
#  ans <- st_distance(mp1, mp1, by_element = FALSE)

  # ref https://stackoverflow.com/questions/49532911/calculate-distance-longitude-latitude-of-multiple-in-dataframe-r:
  # poi <- df1 # misses 'id'.

  # ref https://stackoverflow.com/questions/50372533/changing-crs-of-a-sf-object
  # > roads <- st_transform(roads, crs = 4326)
  # [S3 method for s2_geography
  #   st_as_sf(x, ..., crs = st_crs(4326))
  # ]
# coerce to sf object
poi <- st_as_sf(x=df1, coords=longLat, crs=st_crs("WGS84"))
  # mp2 <- st_multipoint(poi) # [Error in valid_numeric_matrix(x) : is.numeric(x) is not TRUE]

# st_crs(poi) = "WGS84" # 4326 # EPSG:4326 ref https://r-spatial.github.io/sf/reference/st_crs.html.
  # 4326 adds "distance" column.
str(poi)
  return(poi)
}
pairDist <- function(df1, longLat=cLongLat){
  poi <- mySpots(df1, longLat=longLat)
  ans2 <- st_distance(poi, by_element = FALSE)
  ans2 <- round(ans2) # (in standard metres as integer) as expected by TSP.
  str(ans2)

# # duplicate object with shifted rows
# poi2 <- poi[c(1, 1:(nrow(poi) - 1)), ]

# # compute pairwise distance
# distance <- st_distance(x = poi, y = poi2)

# # extract only the diagonal corresponding to the distance between row r and row r+1
# distance <- diag(distance)

# # add result to data
# poi$distance <- distance

# # first distance to NA
# poi$distance[1] <- NA
#   ans <- poi$distance

  return(ans2)
}
mapTour <- function(tour, df1, charTitle, longLat=cLongLat, proj4string="+proj=longlat +datum=WGS84",
  xlim=c(-166,-47), ylim=c(15,83)){
  # adapted from https://cran.r-project.org/package=TSP.

# data("USCA50")
# dist.mx <- USCA50
# data("iris")
# d <- dist(iris[-5])
# data("USCA312_GPS") # with lat long.
# head(USCA312_GPS)

  # create spatial coordinates and a basemap using WGS84 projection.
  USCA312_coords <- SpatialPointsDataFrame(df1[, longLat],
    # cbind(USCA312_GPS$long, USCA312_GPS$lat),
    proj4string=CRS(proj4string), data=df1)
  USCA312_basemap <- map2SpatialLines(map("world",
    xlim=xlim, ylim=ylim, plot=FALSE),
    proj4string=CRS(proj4string))
  ## plot map
  plot(as(USCA312_coords, "Spatial"), axes=TRUE)
  plot(USCA312_basemap, add=TRUE, col="gray")
  ## plot tour and add cities
  tour_line <- SpatialLines(list(Lines(list(
    Line(USCA312_coords[c(tour, tour[1]),])), ID="1")))
  plot(tour_line, add=TRUE, col="red")
  points(USCA312_coords, pch=3, cex=0.4, col="black")
  text(USCA312_coords, label=tour) # INGUJ.df[, cLongLat]
  title(main=charTitle)
  # > plot(df1[,c("lat","long")])
  # > points(df1[,c("lat","long")], pch=as.character(1:nrow(df1)), cex=1, col="red") # Beware: drops digits >9!
  return()
}
map_country <- function(country, x_limits = NULL, y_limits = NULL){
  # copied from https://www.r-bloggers.com/2022/10/map-any-region-in-the-world-with-r-part-i-the-basic-map/
    ## Verifying the arguments passed to the function
    if(!is.character(country)) stop("Name of the country should be character")
    if(length(country) != 1) stop("Function supports only one country per map")
    ## Load libraries
    require(maps)
    require(ggplot2)
    if(!country %in% map_data('world')$region) stop('Country name not recognized\nTo see a list of recognized countries run <unique(maps::map_data("world")$region)>')
    ## If coords limits missing, print worldwide map with coordinates system to allow
    ## User observe coords for reference
    if(missing(x_limits) || missing(y_limits)) {
        warning("X and/or Y limits not provided.\nPrinting worldwide map.")
        map_country_theme <- theme(panel.background = element_rect(fill = '#4e91d2'))
    }
    else {
        if(length(x_limits) != 2 || length(y_limits) != 2 ||
           !all(grepl('^-?[0-9.]+$', c(x_limits, y_limits)))){
            stop("Limits for X and Y coords should be provided as vectors with two numeric values")
        }
        else {
            ## All the received inputs are correct.
            ## Let's define our custom theme for the final map 
            map_country_theme <- theme_bw() +
                theme(panel.background = element_rect(fill = '#4e91d2'),
                      legend.position = 'none',
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.line = element_line(colour = "black"),
                      axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
        }
    }
    ## make a df with only the country to overlap
    map_data_country <- map_data('world')[map_data('world')$region == country,]
    ## The map (maps + ggplot2 )
    ggplot() +
        ## First layer: worldwide map
        geom_polygon(data = map_data("world"),
                     aes(x=long, y=lat, group = group),
                     color = '#9c9c9c', fill = '#f3f3f3') +
        ## Second layer: Country map
        geom_polygon(data = map_data_country,
                     aes(x=long, y=lat, group = group),
                     color = '#4d696e', fill = '#8caeb4') +
        coord_map() +
        coord_fixed(1.3,
                    xlim = x_limits,
                    ylim = y_limits) +
        ggtitle(paste0("A map of ", country)) +
        scale_x_continuous(n.breaks = 20) +
        scale_y_continuous(n.breaks = 20) +
        map_country_theme
}
## Test the function with a different country
# map_country("Germany", c(-2, 22), c(47, 55))
# map_country("India", c(65, 100), c(5, 35))
which.FUN.mx <- function(fn, mx){
    whats.fn <- do.call(fn, list(mx))
    ij.mx <- which(mx == whats.fn, arr.ind=TRUE) # Beware: this can return a matrix of indices!
    return(ij.mx)
}
edgeDist.max <- function(tour, tsp.mx){
  tour.edges <- matrix(c(tour, c(tour[-1], tour[1])), nrow=length(tour), ncol=2)
  # distances between adjacent nodes on tour.
  str(tour.edges)
  te.dist <- apply(tour.edges, MARGIN=1, FUN=function(ijVec){ tsp.mx[ijVec[1], ijVec[2]] })
  str(te.dist)
  tour.edges.dist <- cbind(tour.edges, te.dist)
  ans <- tour.edges.dist[which.max(tour.edges.dist[, "te.dist"]),]
  return(ans)
}


mustInstall <- FALSE
if(mustInstall){
  install.packages("TSP")
  # install.packages("TSP", repos = "https://mhahsler.r-universe.dev")
} # else continue.

library("TSP")
library(sf)
require(sp)
require(maps)
require(gpclib)
require(rgeos)
require(maptools)
## set path to the Concorde executible if it is not in the search PATH
## Example:
## concorde_path("~/concorde/")
concorde_path("F://www-since2022Nov/concorde-linkern/") # returns message [found: concorde.exe linkern.exe]
concorde_path()
inGPS <- 2
fName <- c("dist", "INGUJ50_GPS")[inGPS]
