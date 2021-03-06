Visualization Examples
=========================

**OVERVIEW**

This repository is a collection of code that I have written at the Woods Hole Research Center using the R programming language. R is a scripting language similar to Python.

Additionally, I have included a python script (`other/satellite_image_inventory.py`) that I wrote as part of my first project working at the Woods Hole Research Center in 2012. 

**PROJECTS**

**Climate**

`climate/plot_clim_data.R`

- Historical and projected climate for the Appalachian Landscape Conservation Cooperative (ALCC)
- Processed data seperately on NASA's Advanced Supercomputer (NAS), Pleiades
	- Extracted climate data for each park and "protected area centered ecosystem" (PACE; area directly surrounding the park)

<img src='/climate/plots/alcc_park_prism_dcp30_plots_metric.png?raw=true'>

*Past (PRISM and NEX DCP-30 historical; purple and pink, respectively) and future (NEX DCP-30 RCP 4.5 and RCP 8.5; green and blue, respectively) temperature and precipitation data for the Deleware Water Gap (DEWA), Great Smokey Mountain (GRSM) and Shenendoah (SHEN) national parks.*

**Permafrost**

`permafrost/plot_profile.R`

- Temperature profiles for two permafrost bore holes (u70 and u71) in northern Alaska

<img src='/permafrost/fig_2_tsp_freeze_thaw_depth_temp.png?raw=true'>

*Temperature depth profiles of two permafrost bore holes on the north slope of Alaska. The active layer depth (point where the ground thaws) is shown by the transition from cyan to yellow.*

**Other Scripts, Plots & Maps**

`satellite_image_inventory.py` crawls through the Woods Hole Research Center's on-site servers, identifies spatial data, and extracts metadata to include in a PostgreSQL (geospatial SQL) database.

`solar_insolation_hillshade_hourly.R` calculates solar insolation from digital elevation maps (DEMs) using solar radiation calculations. This script is designed to work on NASA's Advanced Supercomputer (NAS), Pleiades. 

I created the following figures for a study that I led last year (Guay *et al.* 2014). 

<img src='/other/guay_et_al_fig3.tiff?raw=true' width='50%'>

*Difference between two versions of the GIMMS NDVI product (GIMMSg and GIMMS3g). Black line shows trend from 1982 to 2008. Dotted blue lines show trends before and after an increase in the ∆NDVI around 1997.*

<img src='/other/guay_et_al_sup_ndvi_plot_nbar.tiff?raw=true' width='50%'>

*(a) NDVI and NDVI trends for GIMMSg, GIMMS3g, MODIS NBAR, MODIS, SeaWiFS, SPOT D10 and SPOT S10. (b) NDVI and NDVI trends between 2002 and 2008 (i.e. the common record between all sensors).*

<img src='/other/guay_et_al_fig5.tiff?raw=true' width='50%'>

*Agreement between NDVI trend for GIMMS3g, MODIS NBAR, and SPOT D10 from 2002 to 2008. Black dots indicate significant agreement or disagreement (p < 0.05). Agricultural lands and areas north of 72°N are masked (white).*
