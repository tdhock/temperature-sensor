figure-timeseries/index.html: figure-timeseries.R temperature.RData
	R --no-save < $<
temperature.RData: temperature.R ../time_degreesC.log
	git add history/*.json
	R --no-save < $<

