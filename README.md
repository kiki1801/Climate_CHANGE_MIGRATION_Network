# Climate ChanGe MiGration Network (CLIMINET)

Here, find all files used for **CLIMINET:**

- **Données** includes all raw data files:
  - **/Geo** contains GeoSpatial Data from various sources ([Global Administrative Areas (GADM)](https://gadm.org/download_world.html), [Natural Earth](https://www.naturalearthdata.com/), [FAO](https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/home), [UN | GeoSpatial Hub](https://geoservices.un.org/webapps/geohub/), [WHO](https://gis-who.hub.arcgis.com/) and [World Bank](https://datacatalog.worldbank.org/search/dataset/0038272/World-Bank-Official-Boundaries));
  - **/IMS_from_ONU** contains International MiGration Stock Data from [UN | Population Division](https://www.un.org/development/desa/pd/content/international-migrant-stock);
  - **/REGIONs** contains countries' information from [UN | Statistics Division](https://unstats.un.org/unsd/methodology/m49/) and [World Bank](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups).
- **R** includes files that explain how we create some data and all the source code:
  - **/CCMN_GeoDATA** explain how we create our own GeoSpatial Data from raw ones;
  - **/CCMN_IMS_from_ONU** explain how we create an enhanced version of International MiGration Stock Data, which incorporates new variables;
  - **/UI_and_SERVER** contains **CLIMINET Website** and **Data Exploration Application (DEA)** source code.

*Our own GeoSpatial Data and the enhanced version of International MiGration Stock Data are located in **R/UI_and_Server/CLIMINET/Données**.*

**All this work fits into a broader framework &#8658; [Belmont Forum](https://www.belmontforum.org/archives/projects/international-migration-climate-change-and-network-effects-a-worldwide-study).**

To reproduce CLIMINET's R environment `renv.lock` file is available in R/UI_and_SERVER/CLIMINET. To use `renv.lock` file, run next command:
   ```r
   renv::restore()
   ```
