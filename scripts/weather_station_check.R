# Check the closet weather station for sites with multiple stations

library(geosphere)

# USC00046646: Lat: 37.4436, Lon: -122.1402	USC00047339: Lat: 37.4767, Lon: -122.2386
# 333 Ravenswood Ave. Menlo Park, CA 94025 # -122.175210, 37.457190
# c(lon, lat)
distm(c(-122.1402, 37.4436), c(-122.175210, 37.457190), fun = distHaversine) # smaller USC00046646
distm(c(-122.2386, 37.4767), c(-122.175210, 37.457190), fun = distHaversine)


# 1777 Exposition Drive Boulder, CO 80301 # lon -105.247110, 40.017600
# USC00050848:  Lat: 39.9919° N Lon: -105.2667 	USC00053629: Lat: 39.9363° N Lon: -105.3502	USR0000CBDR: Lat: 40.0181° N Lon: -105.3614
distm(c(-105.2667, 39.9919), c(-105.247110, 40.017600), fun = distHaversine) #  smallest
distm(c(-105.3502, 39.9363), c(-105.247110, 40.017600), fun = distHaversine) #  
distm(c(-105.3614, 40.0181), c(-105.247110, 40.017600), fun = distHaversine) #  

# 501 Chipeta Way Salt Lake City, UT 84108 # lon -111.822960, lat 40.758930
# USW00024127: Lat: 40.7707° N Lon: -111.9650	#
# USC00420819: Lat: 40.8910° N Lon: -111.8504	#
# USC00421446: Lat: 40.8150° N Lon: -111.8321	#
# USC00421759: Lat: 40.6189° N Lon: -111.7836	#
# USC00425892: Lat: 40.7485° N Lon: -111.7232	#
# USS0011J69S: Lat: 40.8300° N Lon: -111.7600	#
# USS0011J37S: Lat: 40.8700° N Lon: -111.7200	#
# USS0011J42S: Lat: 40.5600° N Lon: -111.6600	#
# USS0011J64S: Lat: 40.8400° N Lon: -111.7100	#
# USS0011J65S: Lat: 40.6600° N Lon: -111.6400
distm(c(-111.9650, 40.7707), c(-111.822960, 40.758930), fun = distHaversine)
distm(c(-111.8504, 40.8910), c(-111.822960, 40.758930), fun = distHaversine)
distm(c(-111.8321, 40.8150), c(-111.822960, 40.758930), fun = distHaversine) #smallest
distm(c(-111.7836, 40.6189), c(-111.822960, 40.758930), fun = distHaversine)
distm(c(-111.7232, 40.7485), c(-111.822960, 40.758930), fun = distHaversine)
distm(c(-111.7600, 40.8300), c(-111.822960, 40.758930), fun = distHaversine)
distm(c(-111.7200, 40.8700), c(-111.822960, 40.758930), fun = distHaversine)
distm(c(-111.6600, 40.5600), c(-111.822960, 40.758930), fun = distHaversine)
distm(c(-111.7100, 40.8400), c(-111.822960, 40.758930), fun = distHaversine)
distm(c(-111.6400, 40.6600), c(-111.822960, 40.758930), fun = distHaversine)