---
title: "Exploring interactive mapping with mapboxgl"
description: "This post explores some mapboxgl features"
author:
  - name: Mickey Rafa
    url: https://mrafa3.github.io/
    #orcid: 0000-0002-5300-3075
date: 07-30-2024
categories: [R, tidyverse, mapping] # self-defined categories
#citation: 
 # url: https://mrafa3.github.io/posts/2024-07-15-wikipedia-international-mens-soccer/ 
image: map_example.png
draft: true # setting this to `true` will prevent your post from appearing on your listing page until you're ready!
editor: 
  markdown: 
    wrap: 72
---


[!Muggins Mountain Wilderness](example_map.png){width=300}

# Introduction

I wanted to experiment with the `mapboxgl::` package to see what kinds of interactive maps I could produce in R. I've seen lots of great posts from Kyle Walker on the functionality of this new package, and I wanted to give it a try.

## Setup

**Required packages**:


::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(mapgl)
```
:::

I followed Kyle's instructions on [this Youtube video](https://www.youtube.com/watch?v=n71qnOQD-d4) (and plan to purchase his material for deeper learning).

## Mapping

Steps: 

1.  Create a Mapbox account.
2.  Generate an access key.
3.  Using the `usethis::edit_r_environ()` function, add the Mapbox access token.
4.  Render the map as shown in the demo.


::: {.cell}

```{.r .cell-code}
mapboxgl(
  style = mapbox_style('satellite-streets'),
  center = c(-114.26608, 32.7213),
  zoom = 14,
  pitch = 80,
  bearing = 41
) |>
  add_raster_dem_source(
    id = 'mapbox-dem',
    url = 'mapbox://mapbox.mapbox-terrain-dem-v1',
    tileSize = 512,
    maxzoom = 14
  ) |>
  set_terrain(
    source = 'mapbox-dem',
    exaggeration = 1.5
  )
```

::: {.cell-output-display}

```{=html}
<div id="htmlwidget-0ebeb27d5b39ff042256" style="width:100%;height:480px;" class="mapboxgl html-widget"></div>
<script type="application/json" data-for="htmlwidget-0ebeb27d5b39ff042256">{"x":{"style":"mapbox://styles/mapbox/satellite-streets-v12","center":[-114.26608,32.7213],"zoom":14,"bearing":41,"pitch":80,"projection":"globe","parallels":null,"access_token":"pk.eyJ1IjoibWlja2V5cmFmYSIsImEiOiJjbHpkM3FiaW4waXhyMmlxMngxeDFybGZtIn0.WWIOvQt35xd0ctIwqWPQ2A","additional_params":[],"sources":[{"id":"mapbox-dem","type":"raster-dem","url":"mapbox://mapbox.mapbox-terrain-dem-v1","tileSize":512,"maxzoom":14}],"terrain":{"source":"mapbox-dem","exaggeration":1.5}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


There's also functionality to overlay data (such as Census data), which would be a neat continuation.



