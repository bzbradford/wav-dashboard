**Aug & Nov 2024**

* Update baseline and nutrient data

**Feb 2023**

* Add buttons to download entire baseline and nutrient datasets
* Some cosmetic improvements to download sections
* Improve thermistor plot behavior across deployment gaps when showing multiple years
* Fix mean temperature calculation for report's thermistor plot
* Improve station lists tab with more data columns, download buttons, and station selection buttons.

**Jan 2023**

* Final adjustments for PDF report generation.
* Modify some app language and interpretations to match the station reports.
* Add natural community interpretation to thermistor page.
* Combine UI for the thermistor monthly, daily, and hourly data tables.
* Switch air/water temp to default to Fahrenheit.

**Dec 2023**

* Develop downloadable PDF reports feature, pending install of pdf engine on server
* Final update of baseline, nutrient, and thermistor data for the year

**Nov 2023**

* Add user location feature - house icon and popup with watershed information
* Station points are now colored based on which station types are shown
* Can select stations with data from most recent 4 years + all prior (as a group)
* Add stream depth to baseline data summary table
* Added ability to screenshot entire page and download as PNG. Cannot render the map correctly at this time so the map is excluded from the rendered image.
* Allow the last selected station to remain selected when no stations are available on the map due to filters. The station selection list and data tabs remain populated even when the map has all stations cleared out.

**Oct 2023**

* Fix issue with recently viewed station selection buttons not working properly.
* When currently selected station is no longer available due to map selections, the nearest available station is selected, not a random one.
* The Next/Prev station buttons now go to the next closest station to the East or West.
* The map doesn't re-center on a station unless it is clicked or is off screen.
* Added satellite imagery as a basemap option on the map.
* Added a landscape composition comparison plot on the watersheds/landscape tab and ability to download an image of the landscape plots.
* Added baseline monitoring data collected between 2015 and the present date to the dashboard.
* Some tweaks to the baseline data chart to improve readability when many years are displayed. Also enabled selecting a discrete date range.
* Added more station watershed and location context information to the watershed information tab.

**May 2023**

* Added a 'bookmarking' toggle, where if enabled, the station id is shown in the browser address bar, and the station title is shown in the page title. This allows sharing or bookmarking a specific station to easily select that station when loading the dashboard.

**Apr 2024**

* Added the watershed info tab, which gives a summary of the names and spatial extents of the watersheds in which a site is located. Users can also view landscape data for the watershed surrounding the currently selected station and compare it to the Wisconsin average, for subwatersheds, watersheds, and subbasins.

**Mar 2023**

* Added language to the total phosphorus plot matching the DNR's exceedance criteria based on the confidence limit and median observation.

**Feb 2023**

* Added monthly water temperature summary to thermistor page.

**Dec 2022**

* Added additional 2022 baseline data exported from the DNR SWIMS database.
* Added a station data summary showing min/max values for baseline parameters.
* Added nutrient data for 2022, which included 112 stations.
* Added arrow indicating current selection to the recent stations list and button to clear the list.
* Removed detailed popups from map when first clicking a point; click the point again once selected to bring up the detailed popup.

**Nov 2022**

* Added thermistor data for 2022. 50 thermistors were deployed, generally starting in early May and recovered in mid-October for an average deployment length of 165 days.
* Added this changelog section to track updates and additions to the dashboard.
