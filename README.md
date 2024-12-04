> [!IMPORTANT]  
> This is work-in-progres.
>
> December 4, 2024: Create the .Rmd, no code insofar.

# afriroads

This project is in two parts: 
1. Build a road network
2. Define points in Africa
3. Find the shortest paths between two points

I present how to use Jedwab and Storeygard (2022, Journal of European Economic Association) road dataset, in `.shp` format 
to build a network of roads in Sub-Saharian Africa. 
Then, I show how to define points in Africa, based on the Ethnographic map of Murdock (1958).
Finally, I present how to run a shortest-path algorithm,
that takes into account distance and speed to join two given points in Africa.

> This repository is prepared by [Matéo Moglia](www.mateomoglia.github.io) and scripted in R.
> 
> Any comment is more than welcomed. 

## Initialization

### Required packages

| Package | Use |
| -------- | ------- |
| `tidyverse` | Standard data manipulation |
| `sf` | Standard spatial package |
| `igraph` | Graph-based network analysis |
| `terra` | Raster manipulation |
| `afrilearnr` | Obtain African countries shapes
### Required data

Data are from [Jedwab and Storeygard (2022)](https://doi.org/10.1093/jeea/jvab027) replication package. 
I extract the files `roads_zone_remi_polyline.shp` and `roads_zone_polyline.shp`. 

Ethnic group data are from Nunn (2004) based on Murdock (1957). 

Finally, population data are from WorldPop Gridded Data. 

| Data | Format | Source |
| -------- | ------- | ------- |
| Road network | `shp`| Jedwab and Storeygard (2022) |
| Murdock ethnic group | `shp` | Nunn (2004) |
| Population | `tiff` |  WorldPop 2010 |
| Africa shapes | `shp` | `afrilearnr` package 

### Project organization 

The project tree should looks like the following.

```
/transafrican
|    master.R
└─── code
   └─── 0_data_processing
           0_open_jedwab_storeygard.R
           0_open_murdock_ethnic_groups.R
   └─── 1_build_network
           1_make_graph.R
   └─── 2_define_points
           2_define_centroids.R
           2_define_cities.R
   └─── 3_make_output
└─── output 
    └─── data
    └─── graph
    └─── map
    └─── table
└─── raw 
    └─── afrileanr
    └─── ethnies
    └─── JedwabStoreygardJEEA
└─── temp 
    └─── centroids 
    └─── distances 
    └─── kmean
    └─── shapefiles
```

A `master` script is present to run the whole analysis, set the paths and the required packages. 

Once the data are created using `0_data_processing`, `1_build_network` and `2_defined_points`, one can run independantly `3_make_output`.

> The project takes about 12 hours to run on a standard laptop (MacBook Pro i5 8GB 2019). 

