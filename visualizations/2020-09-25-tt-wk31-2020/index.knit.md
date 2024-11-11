---
title: "Analyzing size distributions in the Palmer Penguins dataset"
# description: "Visualizing distributions with some alternative chart types"
author:
  - name: Mickey Rafa
    url: https://mrafa3.github.io/
date: 09-25-2020
categories: [R, "#TidyTuesday", ternary-plot, ridge-plot] # self-defined categories
image: "tt_2020_31_ternary_thumbnail.png"
draft: true # setting this to `true` will prevent your post from appearing on your listing page until you're ready!

format:
  html:
    toc: true
    toc-depth: 5
    code-link: true
    code-fold: true
    code-tools: true
    code-summary: "Show code"
    self-contained: true

editor_options: 
  chunk_output_type: inline
  
execute: 
  error: false
  message: false
  warning: false
  eval: true
---



![](){#fig-1}

# 1. Load Packages & Setup



::: {.cell}

```{.r .cell-code}
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,
    tidytuesdayR,
    dlookr,
    ggtext,
    gt,
    gtExtras,  #for font awesome icons in gt tables
    ggbump,
    showtext,
    janitor,   #for clean_names()
    scales,
    htmltools, #for tagList()
    glue,
    here,
    ggridges,
    ggtern,
    geomtextpath
)    

font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add_google("Lato")
```
:::



# 2. Read in the Data



::: {.cell}

```{.r .cell-code}
tt_year <- 2020
tt_week <- 31

tuesdata <- tidytuesdayR::tt_load(tt_year, week = tt_week)

penguins <- tuesdata$penguins
penguins_raw <- tuesdata$penguins_raw
```
:::



# 3. Examine the Data 



::: {.cell}

```{.r .cell-code}
penguins %>% 
  glimpse()
```

::: {.cell-output .cell-output-stdout}

```
Rows: 344
Columns: 8
$ species           <chr> "Adelie", "Adelie", "Adelie", "Adelie", "Adelie", "A…
$ island            <chr> "Torgersen", "Torgersen", "Torgersen", "Torgersen", …
$ bill_length_mm    <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, …
$ bill_depth_mm     <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18.1, …
$ flipper_length_mm <dbl> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190, 186…
$ body_mass_g       <dbl> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 3475, …
$ sex               <chr> "male", "female", "female", NA, "female", "male", "f…
$ year              <dbl> 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007…
```


:::

```{.r .cell-code}
penguins_raw %>% 
  glimpse()
```

::: {.cell-output .cell-output-stdout}

```
Rows: 344
Columns: 17
$ studyName             <chr> "PAL0708", "PAL0708", "PAL0708", "PAL0708", "PAL…
$ `Sample Number`       <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
$ Species               <chr> "Adelie Penguin (Pygoscelis adeliae)", "Adelie P…
$ Region                <chr> "Anvers", "Anvers", "Anvers", "Anvers", "Anvers"…
$ Island                <chr> "Torgersen", "Torgersen", "Torgersen", "Torgerse…
$ Stage                 <chr> "Adult, 1 Egg Stage", "Adult, 1 Egg Stage", "Adu…
$ `Individual ID`       <chr> "N1A1", "N1A2", "N2A1", "N2A2", "N3A1", "N3A2", …
$ `Clutch Completion`   <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", …
$ `Date Egg`            <date> 2007-11-11, 2007-11-11, 2007-11-16, 2007-11-16,…
$ `Culmen Length (mm)`  <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34…
$ `Culmen Depth (mm)`   <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18…
$ `Flipper Length (mm)` <dbl> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190,…
$ `Body Mass (g)`       <dbl> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 34…
$ Sex                   <chr> "MALE", "FEMALE", "FEMALE", NA, "FEMALE", "MALE"…
$ `Delta 15 N (o/oo)`   <dbl> NA, 8.94956, 8.36821, NA, 8.76651, 8.66496, 9.18…
$ `Delta 13 C (o/oo)`   <dbl> NA, -24.69454, -25.33302, NA, -25.32426, -25.298…
$ Comments              <chr> "Not enough blood for isotopes.", NA, NA, "Adult…
```


:::

```{.r .cell-code}
penguins %>% 
  summary()
```

::: {.cell-output .cell-output-stdout}

```
   species             island          bill_length_mm  bill_depth_mm  
 Length:344         Length:344         Min.   :32.10   Min.   :13.10  
 Class :character   Class :character   1st Qu.:39.23   1st Qu.:15.60  
 Mode  :character   Mode  :character   Median :44.45   Median :17.30  
                                       Mean   :43.92   Mean   :17.15  
                                       3rd Qu.:48.50   3rd Qu.:18.70  
                                       Max.   :59.60   Max.   :21.50  
                                       NA's   :2       NA's   :2      
 flipper_length_mm  body_mass_g       sex                 year     
 Min.   :172.0     Min.   :2700   Length:344         Min.   :2007  
 1st Qu.:190.0     1st Qu.:3550   Class :character   1st Qu.:2007  
 Median :197.0     Median :4050   Mode  :character   Median :2008  
 Mean   :200.9     Mean   :4202                      Mean   :2008  
 3rd Qu.:213.0     3rd Qu.:4750                      3rd Qu.:2009  
 Max.   :231.0     Max.   :6300                      Max.   :2009  
 NA's   :2         NA's   :2                                       
```


:::
:::

::: {.cell}

```{.r .cell-code}
penguins %>% 
  mutate_if(is.numeric, scale) %>% 
  summarise_if(is.double, list(min, max), na.rm=TRUE)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 10
  bill_length_mm_fn1 bill_depth_mm_fn1 flipper_length_mm_fn1 body_mass_g_fn1
               <dbl>             <dbl>                 <dbl>           <dbl>
1              -2.17             -2.05                 -2.06           -1.87
# ℹ 6 more variables: year_fn1 <dbl>, bill_length_mm_fn2 <dbl>,
#   bill_depth_mm_fn2 <dbl>, flipper_length_mm_fn2 <dbl>,
#   body_mass_g_fn2 <dbl>, year_fn2 <dbl>
```


:::
:::



# 4. Tidy the Data 



::: {.cell}

:::



# 5. Visualization Parameters 



::: {.cell}

```{.r .cell-code}
my_theme <- theme(
  text = element_text(family = 'Lato', size = 14), 
  # plot.title = element_text(family="Gill Sans MT", color="black", face="bold", size=24, hjust=0), 
  # plot.subtitle = element_text(family="Gill Sans MT", color="black", size=12, hjust=0), 
  # axis.title = element_text(family="Gill Sans MT", color="black", face="bold", size=12), 
  # axis.text = element_text(family="Gill Sans MT", color="black", size=18), 
  axis.ticks = element_blank(), 
  # plot.caption = element_text(family="Gill Sans MT", color="black", size=12), 
  plot.background = element_blank(),
  panel.background =  element_blank(), 
  legend.background = element_blank(), 
  panel.grid.major = element_line(colour = "grey90", size = 0.5),
  panel.grid.minor = element_line(colour = "grey93", size = 0.5),
  panel.border = element_blank(), 
  #panel.border = element_rect(colour = "black", size = 0.5, fill=NA, linetype = 1),
  legend.title=element_blank(), 
  # legend.text = element_text(family="Gill Sans MT", color="black", size=12, hjust=0),
  legend.position = 'top')

peng_cols <- c('Biscoe' = 'cornflowerblue', 
                 'Dream' = 'seagreen3',
                 'Torgersen' = 'maroon1',
                 #SPECIES
                 'Adelie' = 'cornflowerblue', 
                 'Chinstrap' = 'seagreen3',
                 'Gentoo' = 'maroon1')
```
:::




# 6. Plot 




::: {.cell}

```{.r .cell-code}
pen_mass_by_island_ridgeplot_viz <- penguins %>% 
  filter(complete.cases(.)) %>% 
  ggplot(.) + 
  ggridges::geom_density_ridges2(aes(x=body_mass_g,
                                    y=island,
                                    fill=island),
                                color='black',
                                bandwidth=195)  + 
  labs(x='\nBody mass (g)',
       y='',
       title = 'Distribution of the body mass of penguins on the Palmer Archipeligo',
       subtitle = glue(
      "on <span style='color:maroon1'>**Torgersen**</span>, <span style='color:seagreen3'>**Dream**</span>, and <span style='color:cornflowerblue'>**Briscoe**</span> islands"
    ),
       caption = 'Tidy Tuesday Week 31 (2020)<br>**Source**: palmerpenguins::') + 
  scale_x_continuous(labels = comma) + 
  scale_fill_manual(values = peng_cols) + 
  my_theme + 
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        plot.title = element_text(face='bold', size=rel(1.5)), 
        plot.subtitle = element_textbox(),
        plot.caption = element_textbox())
```
:::

::: {.cell}

```{.r .cell-code}
penguin_ternary_viz <- penguins %>% 
  filter(complete.cases(.)) %>% 
  ggtern(aes(x = scale(bill_length_mm), 
               y = scale(body_mass_g), 
               z = scale(flipper_length_mm),
               color=species, fill=species)) + 
  geom_point(aes(fill=species), 
             shape = 21, 
             size = 3,
             alpha = .5) + 
  scale_color_manual(values = peng_cols) + 
  scale_fill_manual(values = peng_cols) + 
  labs(title = 'Relationship between bill length, body mass, and flipper length',
       subtitle = glue(
      "for <span style='color:maroon1'>**Gentoo**</span>, <span style='color:seagreen3'>**Chinstrap**</span>, and <span style='color:cornflowerblue'>**Adelie**</span> penguins"
    ),
       x='', y='', z='') + 
  Larrowlab("Bill length (0-100 scaled)\n") + 
  Tarrowlab("Body mass (0-100 scaled)\n") + 
  Rarrowlab("\nFlipper length (0-100 scaled)") + 
  my_theme + 
  theme(tern.axis.arrow.show = TRUE, 
        plot.title = element_text(face='bold', size=rel(1.5), lineheight = 1.5), 
        tern.axis.text = element_text(size = rel(1.5)),
        legend.position = 'none',
        plot.subtitle = element_textbox(),
        plot.caption = element_textbox())
```
:::



# 7. Save 



::: {.cell}

```{.r .cell-code}
# RIDGEPLOT
ggsave(
  filename = glue("tt_{tt_year}_{tt_week}_ridge.png"),
  plot = pen_mass_by_island_ridgeplot_viz,
  width = 10, height = 6, units = "in", dpi = 320
)

# make thumbnail for page
magick::image_read(glue("tt_{tt_year}_{tt_week}_ridge.png")) %>%
  magick::image_resize(geometry = "400") %>%
  magick::image_write(glue("tt_{tt_year}_{tt_week}_ridge_thumbnail.png"))

# TERNARY
ggsave(
  filename = glue("tt_{tt_year}_{tt_week}_ternary.png"),
  plot = penguin_ternary_viz,
  width = 9, height = 9, units = "in", dpi = 320
)

# make thumbnail for page
magick::image_read(glue("tt_{tt_year}_{tt_week}_ternary.png")) %>%
  magick::image_resize(geometry = "400") %>%
  magick::image_write(glue("tt_{tt_year}_{tt_week}_ternary_thumbnail.png"))
```
:::



# 8. Session Info

::: {.callout-tip collapse="true"}
##### Expand for Session Info



::: {.cell}
::: {.cell-output .cell-output-stdout}

```
R version 4.4.0 (2024-04-24)
Platform: aarch64-apple-darwin20
Running under: macOS Sonoma 14.6.1

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Denver
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] geomtextpath_0.1.4 ggtern_3.5.0       ggridges_0.5.6     here_1.0.1        
 [5] glue_1.8.0         htmltools_0.5.8.1  scales_1.3.0       janitor_2.2.0     
 [9] showtext_0.9-7     showtextdb_3.0     sysfonts_0.8.9     ggbump_0.1.0      
[13] gtExtras_0.5.0     gt_0.11.1          ggtext_0.1.2       dlookr_0.6.3      
[17] tidytuesdayR_1.1.2 lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1     
[21] dplyr_1.1.4        purrr_1.0.2        readr_2.1.5        tidyr_1.3.1       
[25] tibble_3.2.1       ggplot2_3.5.1      tidyverse_2.0.0    pacman_0.5.1      

loaded via a namespace (and not attached):
 [1] gridExtra_2.3           httr2_1.0.5             rematch2_2.1.2         
 [4] rlang_1.1.4             magrittr_2.0.3          hrbrthemes_0.8.7       
 [7] snakecase_0.11.1        compiler_4.4.0          systemfonts_1.1.0      
[10] vctrs_0.6.5             crayon_1.5.3            pkgconfig_2.0.3        
[13] fastmap_1.2.0           magick_2.8.5            labeling_0.4.3         
[16] fontawesome_0.5.2       utf8_1.2.4              promises_1.3.0         
[19] rmarkdown_2.28          markdown_1.13           tzdb_0.4.0             
[22] bit_4.5.0               xfun_0.48               jsonlite_1.8.9         
[25] later_1.3.2             parallel_4.4.0          R6_2.5.1               
[28] stringi_1.8.4           pagedown_0.21           compositions_2.0-8     
[31] extrafontdb_1.0         Rcpp_1.0.13             knitr_1.48             
[34] extrafont_0.19          gitcreds_0.1.2          httpuv_1.6.15          
[37] timechange_0.3.0        tidyselect_1.2.1        rstudioapi_0.17.1      
[40] yaml_2.3.10             curl_5.2.3              lattice_0.22-6         
[43] plyr_1.8.9              shiny_1.9.1             withr_3.0.1            
[46] evaluate_1.0.1          bayesm_3.1-6            xml2_1.3.6             
[49] pillar_1.9.0            tensorA_0.36.2.1        generics_0.1.3         
[52] vroom_1.6.5             rprojroot_2.0.4         paletteer_1.6.0        
[55] hms_1.1.3               commonmark_1.9.2        munsell_0.5.1          
[58] xtable_1.8-4            gdtools_0.4.0           tools_4.4.0            
[61] hexbin_1.28.4           robustbase_0.99-4-1     reactable_0.4.4        
[64] grid_4.4.0              Rttf2pt1_1.3.12         gh_1.4.1               
[67] colorspace_2.1-1        proto_1.0.0             cli_3.6.3              
[70] latex2exp_0.9.6         rappdirs_0.3.3          kableExtra_1.4.0       
[73] textshaping_0.4.0       fontBitstreamVera_0.1.1 fansi_1.0.6            
[76] viridisLite_0.4.2       svglite_2.1.3           gtable_0.3.5           
[79] DEoptimR_1.1-3          digest_0.6.37           fontquiver_0.2.1       
[82] farver_2.1.2            htmlwidgets_1.6.4       lifecycle_1.0.4        
[85] mime_0.12               bit64_4.5.2             fontLiberation_0.1.0   
[88] gridtext_0.1.5          MASS_7.3-61            
```


:::
:::


:::

# 9. Github Repository 

::: {.callout-tip collapse="true"}

##### Expand for GitHub Repo
 
[Access the GitHub repository here](https://github.com/mrafa3/mrafa3.github.io)
:::
