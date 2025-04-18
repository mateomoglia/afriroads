> [!IMPORTANT]  
> This is work-in-progres.
>
> December 4, 2024: Create the .Rmd, no code insofar.
> February 5, 2025: Add all the code, update the readme.
> April 17, 2025: Update all code, finish the readme. 

# afriroads

This project is in two parts:

1.   Build a **road network**
2.   Define **points** in Africa
3.   Find the **shortest paths** between two points

I present how to use Jedwab and Storeygard (2022, JEEA) road dataset, in `.shp` format 
to build a network of roads in Sub-Saharian Africa. 
Then, I show how to define points in Africa, based on the Ethnographic map of Murdock (1958).
Finally, I present how to run a shortest-path algorithm,
that takes into account distance and speed to join two given points in Africa.

> This repository has been prepared by [Matéo Moglia](www.mateomoglia.github.io) and is mostly scripted in R.
> 
> Any comment is more than welcomed. 

## Initialization

### Required packages

| Package | Use |
| -------- | ------- |
| `tidyverse` `dplyr` | Standard data manipulation |
| `sf` | Standard spatial package |
| `igraph` `tidygraph` | Graph-based network analysis |
| `terra` | Raster manipulation |
| `flexclust` | Compute the k-mean |
| `afrilearnr` | Obtain African countries shapes |
| `overpass` | To webscrap (OSM) |

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
| Africa shapes | `shp` | `afrileanrn` package |

### Spatial information

The CRS is 4326/WGS84. Units are kilometers.

### Project organization 

The project tree should looks like the following.

```
/afriroads
|    master.R
└─── code
   └─── 0_data_processing
           1_open_road_network.R
           2_covered_murdock_groups.R
   └─── 1_build_network
           1_make_graph.R
   └─── 2_define_points
           0_mock_clustering.R
           1_geographic_centroids.R
           2_population_centroids.R
           3_kmean_centroids.R
           4_africapolis.R
           5_osm.R
   └─── 3_validation_checks
           1_distance_centroids_network.R
           2_distance_centroids_grid_cell.R
           3_population_around_points.R
           4_compare_internal_distances.R
   └─── 4_compute_distance
           0_function_shortest_path.R
           1_add_the_points.R
           2_connect_the_graph.R
           3_compute_internal_time.R
           4_compute_bilateral_distance_matrix.R
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
    └─── cities 
    └─── distances
    └─── shapefiles
```

A `master` script is present to run the whole analysis, set the paths and the required packages. 

Once the data are created using `0_data_processing`, `1_build_network` and `2_define_points`, one can run independantly `4_compute_distance`.

> The data generation takes about 2 hours to run on my laptop (HP Elitebook 32GB). The expected time to run the full shortest path is about 16 hours.
>
> I haven't doublecheck if the code works on other settings. Let me know if not replicable as it is. 

## Roadmap and choices

Going from a raw `.shp` to a fully functional weighted network is quite challenging. There are three main steps:

1.  Obtain a functional network
2.  Define starting and ending points
3.  Compute the shortest paths

The description follows the naming of the scripts.

### Obtain a functional network

#### 0. Data processing

I open the network as a shapefile with `1_open_road_network.R`. It opens the shapefiles from Jedwab and Storeygard, appends them, and saves the road network into `/temp/shapefiles/current_roads_network.shp`. It consists on the current road segments digitized by Jedwab and Storeygard (as of 2012), as well as information on the future Transafrican Highway network. 

In `2_covered_murdock_groups.R`, I select ethnic groups that are covered by at least one road segment. Uncovered ethnic groups are mainly from North Africa and some islands. 

#### 1. Build network

The shapefile is collapsed into two elements, required for a (weighted) network: an **edge** data frame (containing all road segments, their ID, and their length) and a **node** data frame (containing all nodes of the network). The output shapefiles are `/output/data/graph_edges_current_roads.shp` and `/output/data/graph_nodes_current_roads.shp` (as well for the future network).

#### 2. Define points 

So far, we only have a network at hand. We need to add starting and ending points for the shortest path algorithm. These points are:

1.  The **geographic** centroids of each ethnic group

Using `sf::st_centroid()`, I extract the centroids and save them in `/temp/centroids/geographic_centroids.shp`.

2.  The **population**-weighted geographic centroids

For each ethnic group, I compute the weighted $x$ and $y$ coordinates, then compute the centroid. There are saved in `/temp/centroids/weighted_centroids.shp`.

3.  The **k-mean** centroids

FOr each ethnic group, I weight the coordinates by population, then I apply the kmean procedure from package `kcca` to extract 3 clusters per group. Then, I extract each cluster and compute its population-weighted centroid. The clusters shapefiles are in `/temp/centroids/k_clusters_shapefile.shp` and the centroids are in `/temp/centroids/k_centroids.shp`. It takes a solid hour to run. 

4.   **Africapolis** cities

Use as a benchmark to assess the quality of the kmean procedure. However, the coverage is imperfect and do not use them afterwards. Output is in `/temp/cities/africapolis_cities.shp`.

5.  **Open Street Map** cities

Use as a benchmark to assess the quality of the kmean procedure. However, the coverage is imperfect and do not use them afterwards. Output is in `/temp/cities/osm_cities.shp`.

#### 3. Validation checks

A number of validation checks are performed, in particular to show that kmean centroids are close to the road network and that they predict well the population distribution of each centroids.

#### 4. Compute distance

The final part of the code adds the kmean centroids to the network, create artifical linking edges and compute the shortest paths. The script `/4_compute_distance/0_function_shortest_path` defines functions to compute the shortest paths between all possible dyads. I assume that bilateral times and distances are equal. Four classes of output are made: two for the quickest time and two for the shortest distance, as well as one for each weighted with speed (resp. distance) and without the weighting. Each output includes a $n\times (n-1)/2$ dataset with all paths and a `.csv` matrix of summary statistics.

Scripts `/4_compute_distance/1_add_the_points` and `/4_compute_distance/1_connect_the_graph` respectively add the points as new nodes. Then, those points are connected iteratively to the closest nodes. It takes around an hour to run.

A last step consists in computing the internal time, i.e. the time to go from centroid to itself. I apply three methods: the Head and Mayer (2005) formula, the speed/length of the longest road segment in the cluster, and the (population-weighted) average of all roads' speed/length in the cluster. The script is `/4_compute_distance/3_compute_internal_time`.

Finally, the script `/4_compute_distance/4_compute_bilateral_distance_matrix` allows to compute the shortest path depending on the chosen counterfactual. Computing each counterfactual can take up to several hours. 

Three types of counterfactuals are possible:

1.  Choose a common level of speed for all roads (*baseline*)
2.  Simulate the TransAfrican Highway network completion and associated speed (*tah*)
3.  Simulate the increase of speed for the path between two or more ethnic groups (*specific*). 

For each counterfactual, it is possible to select only a subset of ethnic groups between each it should compute bilateral distances. Mechanically, it reduces total time.

This latter function can be run independantly from the rest.