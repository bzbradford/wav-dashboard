## LEARN MORE TAB ##

learnMoreUI <- function() {
  tagList(
    h3("Monitoring Station Types"),
    p("The sites on the map above show established sites where Water Action Volunteers made water quality monitoring measurements. Stations are only shown if we have data available for that station. Stations may have data for more than one kind of the following data collection protocols:"),
    p(strong("Baseline monitoring:"), "Volunteers enter the WAV program by training to do baseline stream monitoring. Each year, baseline volunteers journey to their monitoring sites once per month from May to October to collect four baseline parameters: dissolved oxygen, instantaneous temperature, transparency and streamflow. During at least two of these months (May/June and September/October), volunteers also collect macroinvertebrates to calculate a biotic index score. Once per season, some advanced volunteers also conduct a habitat assessment."),
    p(strong("Nutrient monitoring:"), "After at least one season of baseline monitoring, some WAV volunteers will support special projects monitoring. Special projects monitoring is designed to either use the same methods as DNR professionals for data collection or to meet specific data needs. Recently these special projects have included monitoring with meters, aquatic invasive species monitoring, nutrient monitoring, and deploying continuous temperature monitors. Nutrient monitoring is the most widespread of the special projects. Volunteers sample for total phosphorus concentrations in rivers and streams. In some instances, volunteers also collect suspended solids samples and/or nitrogen panels."),
    p(strong("Temperature loggers:"), "Across the state there are a number of automatic, deployed temperature loggers that continuously monitor water temperature in streams. This data can be useful for understanding seasonal stream dynamics, as lower temperatures can indicate higher flow rates, more oxygen-rich water, and overall healther stream systems."),

    h3("Map Layers"),
    p(strong("DNR Regions:"), "The Department of Natural Resources has grouped Wisconsin's 72 counties into five different regions, which are shown on the map as a light color fill."),
    p(strong("Nine Key Elements Plans:"), "These are long-term plans for specific watersheds that provide a framework for improving water quality in a holistic manner. The nine elements help assess the contributing causes and sources of nonpoint source pollution, involve key stakeholders and prioritize restoration and protection strategies to address water quality problems. Learn more about NKEs at the", HTML("<a href='https://dnr.wisconsin.gov/topic/Nonpoint/9keyElement' target='_blank'>Wisconsin DNR</a>.")),
    p(strong("HUC8, HUC10, and HUC12 watersheds:"), "HUC stands for Hydrologic Unit Code and is a sequence of numbers or letters that identify a hydrological feature like a river, lake, or drainage basin. For this map, we are including HUC8 boundaries (subbasins), HUC10 boundaries (watersheds), and HUC12 boundaries (subwatersheds) as optional layers so you can better understand the hydrology of Wisconsin. HUC8 is the largest of these classifications, and HUC12 the smallest."),
    p(strong("Station clusters:"), "This is an overlay mode that shows each station as a blue marker or, where many stations are close together (depending on the zoom level on the map), it groups nearby stations together into a clickable zone with a number indicating how many stations are in that region. Regions are calculated automatically with a spatial algorithm. Click on a grouping to zoom in closer."),
    p(strong("Station selection on the map:"), "Use the options in the panel to the right of the map to choose which stations display on the map. Stations can be selected by data type available at that station, or by which year(s) there is data for that station. In addition, the station icons on the map can be colored by station type or by one of several different types of collected data to help visualize regional trends across the state or hone in on regions or stations of interest."),

    h3("About Water Action Volunteers"),
    p("The Water Action Volunteers (WAV) citizen stream monitoring program is an ongoing partnership between the University of Wisconsin–Madison Division of Extension, the Wisconsin Department of Natural Resources (WDNR) and Wisconsin volunteers. The program aims to preserve, protect and restore Wisconsin’s 86,000+ miles of streams and rivers by educating and empowering volunteers to (1) gather high-quality stream data useful for decision-making and natural resource management, and (2) share their data and knowledge. Annually, more than 500 volunteers and an estimated 2,000 supervised students monitor 600+ stream locations throughout the state. Visit the Water Action Volunteers website at", HTML("<a href='https://wateractionvolunteers.org' target='_blank'>wateractionvolunteers.org</a>.")),

    br(),

    bsCollapse(
      bsCollapsePanel("Changelog", includeMarkdown("changelog.md"))
    )
  )
}
