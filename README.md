# SolarCalc

Implementation of NOAA solar calculations  (https://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html) in R. Estimates time of sunrise and sunset based on location and date.

Note that there may be very slight differences in timings (~ 1 minute) compared to those returned in the NOAA Excel spreadsheet. This is because NOAA incorporate time in their calculation of Julian day, whereas this package uses only the date and the time zone.
