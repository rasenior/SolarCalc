#' solar_calc
#'
#' Calculate the time of sunrise and sunset based on date and location.
#' @param latitude Latitude in decimal degrees.
#' @param longitude Longitude in decimal degrees.
#' @param focal_date Date as a character.
#' @param format Format of the date supplied. Defaults to '\%Y-\%m-\%d'.
#' @param tz Time zone expressed as the time difference compared to UTC, in a character format e.g. '+0100' or '-0830'.  Must be five characters, including sign of the time difference and zeros where necessary. Must take account of Daylight Saving Time, where applicable.
#' @param return_var Variables to return: 'both', 'sunrise' or 'sunset'. Defaults to 'both'.
#' @param return_format Whether to return time in conventional format ('norm_time'), or minutes of the day ('day_mins'). Defaults to 'norm_time'.
#' @return Either single result for sunset or sunrise, or a dataframe containing both variables. For simplicity, time is always reported in the format '\%H:\%M', and in the same time zone as the location of interest.
#' @examples
#' solar_calc(latitude = 53.6955847, longitude = -2.0015868, focal_date = '26/06/2017', format = '%d/%m/%Y', tz = '+0100', return_var = 'both')
#' solar_calc(latitude = 52.2099218, longitude = 0.1156409, focal_date = '1950-30-12', format = '%Y-%d-%m', tz = '+0000', return_var = 'sunrise')
#' solar_calc(latitude = 5.9790166, longitude = 116.0711132, focal_date = '01.04.2001', format = '%d.%m.%Y', tz = '+0800', return_var = 'Sunset')
solar_calc <- function(latitude, longitude, focal_date, format = "%Y-%m-%d", tz,
                       return_var = "both", return_format = "date") {
    jd <- julian(as.Date(paste(focal_date, tz), paste(format, "%z")), -2440588)[1]
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
    has <- ha_sunrise(latitude = latitude, sd)

    # Solar noon requires time zone to be a number
    tz <- unlist(strsplit(tz, ""))
    tz_hrs <- as.numeric(paste(tz[1:3], collapse = ""))
    tz_mins <- (as.numeric(paste(tz[c(1, 4:5)], collapse = "")))/60
    tz <- tz_hrs + tz_mins

    sn <- solar_noon(longitude = longitude, et, tz = tz)

    # Define function to convert from minutes to time as character string

    mins2time <- function(day_mins) {
        # Calculate hour
        hour <- floor(day_mins/60)

        # Calculate minutes
        mins <- round(day_mins - (hour * 60), digits = 0)

        # Bind as a time If everything equals zero then strptime will not report time, so it needs to be
        # assigned manually

        if (hour == 0 & mins == 0) {
            norm_time <- "00:00"
        } else {
            norm_time <- paste(hour, mins, sep = ":")

            # Keep only the time element, and coerce back to character string
            norm_time <- strsplit(as.character(strptime(norm_time, "%H:%M")), " ")[[1]][2]
            norm_time <- paste(unlist(strsplit(norm_time, ":"))[1:2], collapse = ":")
        }

        return(norm_time)
    }

    if (tolower(return_var) %in% c("both", "sunrise")) {
        sunrise_mins <- sn - (has * 4)

        # If sunrise should be returned as a date, convert here
        if(tolower(return_format) == "norm_time"){
          sunrise <- mins2time(sunrise_mins)
        }else if(tolower(return_format) == "day_mins"){
          sunrise <- sunrise_mins
        }
    }

    if (tolower(return_var) %in% c("both", "sunset")) {
        sunset_mins <- sn + (has * 4)

        # If sunset should be returned as a date, convert here
        if(tolower(return_format) == "norm_time"){
          sunset <- mins2time(sunset_mins)
        }else if(tolower(return_format) == "day_mins"){
          sunset <- sunset_mins
        }
    }

    if (tolower(return_var) == "both") {
        result <- c(sunrise = sunrise, sunset = sunset)
    } else if (tolower(return_var) == "sunrise") {
        result <- sunrise
    } else if (tolower(return_var) == "sunset") {
        result <- sunset
    }

    return(result)
}
