julian_day <- function(excel_date, time_zone) {
    result <- excel_date + 2415018.5 - (time_zone/24)
    
    return(result)
}


julian_century <- function(julian_day) {
    result <- (julian_day - 2451545)/36525
    
    return(result)
}


geom_mean_long <- function(julian_century) {
    numerator <- 280.46646 + (julian_century * (36000.76983 + julian_century * 0.0003032))
    result <- numerator%%360
    
    return(result)
}

geom_mean_anom <- function(julian_century) {
    result <- 357.52911 + julian_century * (35999.05029 - (0.0001537 * julian_century))
    return(result)
}

eccent_earth_orbit <- function(julian_century) {
    result <- 0.016708634 - julian_century * (4.2037e-05 + (1.267e-07 * julian_century))
    return(result)
}

rad2deg <- function(rad) {
    (rad * 180)/(pi)
}
deg2rad <- function(deg) {
    (deg * pi)/(180)
}

sun_eq_ctr <- function(geom_mean_anom, julian_century) {
    x <- sin(deg2rad(geom_mean_anom)) * (1.914602 - julian_century * (0.004817 + (0.00014 * julian_century)))
    y <- sin(deg2rad(2 * geom_mean_anom)) * (0.019993 - (0.000101 * julian_century))
    z <- sin(deg2rad(3 * geom_mean_anom)) * 0.000289
    
    result <- x + y + z
    return(result)
    
}

sun_true_long <- function(geom_mean_long, sun_eq_ctr) {
    result <- geom_mean_long + sun_eq_ctr
    return(result)
}

sun_true_anom <- function(geom_mean_anom, sun_eq_ctr) {
    result <- geom_mean_anom + sun_eq_ctr
    return(result)
}


sun_rad_vector <- function(eccent_earth_orbit, sun_true_anom) {
    numerator <- 1.000001018 * (1 - (eccent_earth_orbit^2))
    denominator <- 1 + (eccent_earth_orbit * (cos(deg2rad(sun_true_anom))))
    result <- numerator/denominator
    
    return(result)
}

sun_app_long <- function(sun_true_long, julian_century) {
    result <- sun_true_long - 0.00569 - (0.00478 * (sin(deg2rad(125.04 - (1934.136 * julian_century)))))
    return(result)
    
}


mean_obliq_ecliptic <- function(julian_century) {
    x <- 21.448 - (julian_century * (46.815 + (julian_century * (0.00059 - (julian_century * 0.001813)))))
    y <- x/60
    z <- (26 + y)/60
    
    result <- 23 + z
    return(result)
    
}


obliq_corr <- function(mean_obliq_ecliptic, julian_century) {
    result <- mean_obliq_ecliptic + (0.00256 * (cos(deg2rad(125.04 - (julian_century * 1934.136)))))
    return(result)
    
}

sun_rt_ascen <- function(sun_app_long, obliq_corr) {
    x <- cos(deg2rad(sun_app_long))
    y <- cos(deg2rad(obliq_corr)) * sin(deg2rad(sun_app_long))
    z <- atan2(y = y, x = x)
    result <- rad2deg(z)
    
    return(result)
}

sun_declin <- function(obliq_corr, sun_app_long) {
    x <- sin(deg2rad(obliq_corr)) * sin(deg2rad(sun_app_long))
    result <- rad2deg(asin(x))
    
    return(result)
}

var_y <- function(obliq_corr) {
    result <- (tan(deg2rad(obliq_corr/2)))^2
    return(result)
}

eq_time <- function(var_y, geom_mean_long, eccent_earth_orbit, geom_mean_anom) {
    var1 <- var_y * (sin(2 * deg2rad(geom_mean_long)))
    var2 <- 2 * (eccent_earth_orbit * (sin(deg2rad(geom_mean_anom))))
    var3 <- 4 * eccent_earth_orbit * var_y * (sin(deg2rad(geom_mean_anom))) * cos(2 * deg2rad(geom_mean_long))
    var4 <- 0.5 * (var_y^2) * sin(4 * deg2rad(geom_mean_long))
    var5 <- 1.25 * (eccent_earth_orbit^2) * sin(2 * deg2rad(geom_mean_anom))
    
    result <- 4 * rad2deg(var1 - var2 + var3 - var4 - var5)
    
    return(result)
    
}

ha_sunrise <- function(latitude, sun_declin) {
    x <- cos(deg2rad(90.833))/(cos(deg2rad(latitude)) * cos(deg2rad(sun_declin))) - tan(deg2rad(latitude)) * tan(deg2rad(sun_declin))
    
    result <- rad2deg(acos(x))
    
    return(result)
}

solar_noon <- function(longitude, eq_time, timeZone) {
    result <- 720 - (4 * longitude) - eq_time + (timeZone * 60)
    
    return(result)
}





