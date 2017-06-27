#' solar_calc
#'
#' Calculate the time of sunrise and sunset based on date and location.
#' @param latitude Latitude in decimal degrees.
#' @param longitude Longitude in decimal degrees.
#' @param excel_date Date in MS Excel format. See https://github.com/rasenior/ConvertDateTime for functions to convert dates into Excel format.
#' @param time_zone Time zone (numeric), taking account of Daylight saving time where applicable.
#' @param return_var Variables to return: 'both', 'sunrise' or 'sunset'. Defaults to 'both'.
#' @return Either single sunset or sunrise value expressed as minutes of the day, or a dataframe with both variables combined.
#' @examples
#' solar_calc(latitude = 53.6955847, longitude = -2.0015868, excel_date = 42913, time_zone = 1, return_var = 'both')
solar_calc <- function(latitude, longitude, excel_date, time_zone, return_var = "both") {
    jd <- julian_day(excel_date, time_zone)
    jc <- julian_century(jd)
    gml <- geom_mean_long(jc)
    gma <- geom_mean_anom(jc)
    eeo <- eccent_earth_orbit(jc)
    sec <- sun_eq_ctr(gma, jc)
    stl <- sun_true_long(gml, sec)
    sta <- sun_true_anom(gma, sec)
    srv <- sun_rad_vector(eeo, sta)
    sal <- sun_app_long(stl, jc)
    moe <- mean_obliq_ecliptic(jc)
    oc <- obliq_corr(moe, jc)
    sra <- sun_rt_ascen(sal, oc)
    sd <- sun_declin(oc, sal)
    vy <- var_y(oc)
    et <- eq_time(vy, gml, eeo, gma)
    has <- ha_sunrise(latitude, sd)
    sn <- solar_noon(longitude, et, time_zone)
    sunrise <- sn - (has * 4)
    sunset <- sn + (has * 4)

    # Define which results to return
    if (tolower(return_var) == "both") {
        result <- dataframe(sunrise = sunrise, sunset = sunset)
    } else if (tolower(return_var) == "sunrise") {
        result <- sunrise
    } else if (tolower(return_var) == "sunset") {
        result <- sunset
    }

    return(result)
}
