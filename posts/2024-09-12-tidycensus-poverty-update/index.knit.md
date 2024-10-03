---
title: "Reviewing the ACS-1 2023 child poverty estimates"
description: "This post looks at how child poverty estimates have changed in the latest U.S. Census data"
author:
  - name: Mickey Rafa
    url: https://mrafa3.github.io/
    #orcid: 0000-0002-5300-3075
date: 09-12-2024
categories: [R, data-viz, tidyverse, tidycensus, api, purrr] # self-defined categories
#citation: 
 # url: https://mrafa3.github.io/posts/2024-07-15-wikipedia-international-mens-soccer/ 
image: co_comet.png
draft: false # setting this to `true` will prevent your post from appearing on your listing page until you're ready!
code-annotations: hover
editor: 
  markdown: 
    wrap: 72
---



# Introduction

In a [previous tidycensus::
post](https://mrafa3.github.io/posts/2024-08-20-tidycensus-exploration-pt2/),
I showed how to fetch data and do some basic, longitudinal analysis of
U.S. Census data. Today, the Census Bureau released the 1-year estimates
from the American Community Survey (ACS), so I'd like to see how child
poverty seems to be changing from the 2022 to 2023 releases.

## Setup



::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(tidycensus)
library(ggtext)
library(scales) # <1>
library(gt) # <2>
library(glue) # <3>
library(ggforce) # <4>

# census_api_key('INSERT KEY HERE', install = TRUE) # <5>
```
:::



1.  Loading the `scales::` package to transform ggplot scales simply
    (some people choose to explicitly define `scales::` in their code
    rather than loading the library).
2.  The `gt::` library provides functionality for creating ggplot-esque
    tables.
3.  The `glue::` package allows for simple addition of HTML to ggplot
    graphics.
4.  The `ggforce::` package includes a `geom_link()` function, which
    I'll use to create the comet effect in the comet plots that I use.
5.  The first time that you're working with the `tidycensus::` package,
    you need to request an API key at
    https://api.census.gov/data/key_signup.html. The `install=` argument
    will install your personal key to the .Renviron file, and you won't
    need to use the `census_api_key()` function again.



::: {.cell}

:::



# Data

For this analysis, I'm interested in looking at the most recent
state-level child poverty data available from the U.S. Census Bureau,
and I want to construct a longitudinal sense of the change in child
poverty.

First, let's revisit the different American Community Survey products --
**ACS-1** and **ACS-5**.

*What's the difference between these, and how do you choose which survey
product to use for your purposes?*

| Feature | ACS 1-Year Estimates | ACS 5-Year Estimates |
|------------------|----------------------------|---------------------------|
| Data Collection Period | 12 months | 60 months |
| Population Coverage | Areas with 65,000 or more people | All geographic areas, including those with fewer than 65,000 people |
| Sample Size | Smallest | Largest |
| Reliability | Less reliable due to smaller sample size | More reliable due to larger sample size |
| Currency | Most current data | Less current, includes older data |
| Release Frequency | Annually | Annually |
| Best Used For | Analyzing large populations, when currency is more important than precision | Analyzing small populations, where precision is more important than currency |
| Example Usage | Examining recent economic changes | Examining trends in small geographic areas or small population subgroups |

For this post, I am interested in constructing a longitudinal dataset of
the most recent year-on-year estimates, including the 2023 1-year
estimates that were released today. Note that for the ACS-1 products,
estimates are only available for geographic units with populations
greater than 65,000.

As I did in prior posts, I'll use the following series from the American
Community Survey:

-   *B01001_003*: Estimate!!Total:!!Male:!!Under 5 years (all racial
    groups)
-   *B01001_027*: Estimate!!Total:!!Female:!!Under 5 years (all racial
    groups)
-   *B17001_004*: Estimate!!Total:!!Income in the past 12 months below
    poverty level:!!Male:!!Under 5 years
-   *B17001_018*: Estimate!!Total:!!Income in the past 12 months below
    poverty level:!!Female:!!Under 5 years

## Fetching from the `tidycensus::` API



::: {.cell}

```{.r .cell-code}
years <- c(2022, 2023)
```
:::



Next, I'll define a function to fetch the ACS-1 data for 2022 and the
new 2023 estimates.



::: {.cell}

```{.r .cell-code}
fetch_acs_data <- function(year) {
  get_acs(geography = "state", 
          survey = 'acs1',
          variables = c(male_u5_pop = 'B01001_003', 
                        female_u5_pop = 'B01001_027', 
                        male_u5_poverty = 'B17001_004', 
                        female_u5_poverty = 'B17001_018'),
          year = year,
          output = 'wide') %>% 
    mutate(year = year,
           total_u5_popE = male_u5_popE + female_u5_popE, # <6>
           total_u5_povertyE = male_u5_povertyE + female_u5_povertyE, # <6>
           perc_u5_poverty = total_u5_povertyE / total_u5_popE)  # <6>
}

fetch_acs_data_county <- function(year) {
  get_acs(geography = "county", 
          survey = 'acs1',
          variables = c(male_u5_pop = 'B01001_003', 
                        female_u5_pop = 'B01001_027', 
                        male_u5_poverty = 'B17001_004', 
                        female_u5_poverty = 'B17001_018'),
          year = year,
          geometry = TRUE,
          output = 'wide') %>% 
    mutate(year = year,
           total_u5_popE = male_u5_popE + female_u5_popE,
           total_u5_povertyE = male_u5_povertyE + female_u5_povertyE,
           perc_u5_poverty = total_u5_povertyE / total_u5_popE,
           county = str_remove(NAME, " County.*"), # <7>
           state = str_extract(NAME, "[\\w\\s]+$") %>% str_trim()) %>% # <7>
    select(county, state, everything()) %>% 
    group_by(county, state) %>% 
    mutate(perc_u5_poverty_tminus_1 = lag(perc_u5_poverty),
           perc_diff_YoY = perc_u5_poverty - perc_u5_poverty_tminus_1)
}
```
:::



6.  Creating some fields to combine gender-based poverty estimates and
    calculate a percent of the child population measure
7.  Creating `state` and `county` fields to manipulate the dataframe and
    visualize with more simplicity

Then, I'll use `purrr::map_df()` to apply each year to the
`fetch_acs_data()` function that I created, which will result in a
single dataframe of all years.



::: {.cell}

```{.r .cell-code}
combined_acs_data <- map_df(years, fetch_acs_data)
```
:::

::: {.cell}

```{.r .cell-code}
combined_acs_data_county <- map_df(years, fetch_acs_data_county)
```
:::



# Analysis



::: {.cell}

:::



According to the 2023 ACS-1, roughly 113,000 fewer children were below
the Federal Poverty Line.



::: {.cell}
::: {.cell-output-display}

```{=html}
<div id="hxxorclwer" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#hxxorclwer table {
  font-family: 'Roboto Condensed', system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hxxorclwer thead, #hxxorclwer tbody, #hxxorclwer tfoot, #hxxorclwer tr, #hxxorclwer td, #hxxorclwer th {
  border-style: none;
}

#hxxorclwer p {
  margin: 0;
  padding: 0;
}

#hxxorclwer .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: rgba(255, 255, 255, 0);
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: rgba(255, 255, 255, 0);
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hxxorclwer .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hxxorclwer .gt_title {
  color: #333333;
  font-size: 24px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hxxorclwer .gt_subtitle {
  color: #333333;
  font-size: 16px;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hxxorclwer .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hxxorclwer .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hxxorclwer .gt_col_headings {
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: rgba(255, 255, 255, 0);
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hxxorclwer .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 16px;
  font-weight: bolder;
  text-transform: uppercase;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hxxorclwer .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 16px;
  font-weight: bolder;
  text-transform: uppercase;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hxxorclwer .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hxxorclwer .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hxxorclwer .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hxxorclwer .gt_spanner_row {
  border-bottom-style: hidden;
}

#hxxorclwer .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hxxorclwer .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hxxorclwer .gt_from_md > :first-child {
  margin-top: 0;
}

#hxxorclwer .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hxxorclwer .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hxxorclwer .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hxxorclwer .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hxxorclwer .gt_row_group_first td {
  border-top-width: 2px;
}

#hxxorclwer .gt_row_group_first th {
  border-top-width: 2px;
}

#hxxorclwer .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hxxorclwer .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hxxorclwer .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hxxorclwer .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hxxorclwer .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hxxorclwer .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hxxorclwer .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hxxorclwer .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hxxorclwer .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hxxorclwer .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hxxorclwer .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hxxorclwer .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hxxorclwer .gt_sourcenote {
  font-size: 12px;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hxxorclwer .gt_left {
  text-align: left;
}

#hxxorclwer .gt_center {
  text-align: center;
}

#hxxorclwer .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hxxorclwer .gt_font_normal {
  font-weight: normal;
}

#hxxorclwer .gt_font_bold {
  font-weight: bold;
}

#hxxorclwer .gt_font_italic {
  font-style: italic;
}

#hxxorclwer .gt_super {
  font-size: 65%;
}

#hxxorclwer .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hxxorclwer .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hxxorclwer .gt_indent_1 {
  text-indent: 5px;
}

#hxxorclwer .gt_indent_2 {
  text-indent: 10px;
}

#hxxorclwer .gt_indent_3 {
  text-indent: 15px;
}

#hxxorclwer .gt_indent_4 {
  text-indent: 20px;
}

#hxxorclwer .gt_indent_5 {
  text-indent: 25px;
}

#hxxorclwer .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hxxorclwer div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal" style="font-size: 24; font-weight: bold;"><div data-qmd-base64="KipFc3RpbWF0ZWQgY2hpbGQgcG92ZXJ0eSBpbiB0aGUgVS5TLiAoMjAyMi0yMDIzKSoq"><div class='gt_from_md'><p><strong>Estimated child poverty in the U.S. (2022-2023)</strong></p>
</div></div></td>
    </tr>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Total and Percent estimates of those living below the Federal Poverty Line</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 3px; border-bottom-style: solid; border-bottom-color: #000000; font-weight: bold;" scope="col" id="Year">Year</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 3px; border-bottom-style: solid; border-bottom-color: #000000; font-weight: bold;" scope="col" id="Total children &amp;lt; 5 y.o.">Total children &lt; 5 y.o.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 3px; border-bottom-style: solid; border-bottom-color: #000000; font-weight: bold;" scope="col" id="Total children &amp;lt; 5 y.o. living in poverty in last 12 mos.">Total children &lt; 5 y.o. living in poverty in last 12 mos.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 3px; border-bottom-style: solid; border-bottom-color: #000000; font-weight: bold;" scope="col" id="% of children &amp;lt; 5 y.o. living in poverty in last 12 mos.">% of children &lt; 5 y.o. living in poverty in last 12 mos.</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="year" class="gt_row gt_right">2022</td>
<td headers="total_u5_popE" class="gt_row gt_right">18,358,199</td>
<td headers="total_u5_povertyE" class="gt_row gt_right">3,141,107</td>
<td headers="perc_u5_poverty" class="gt_row gt_right">17.1%</td></tr>
    <tr><td headers="year" class="gt_row gt_right">2023</td>
<td headers="total_u5_popE" class="gt_row gt_right">18,333,697</td>
<td headers="total_u5_povertyE" class="gt_row gt_right">3,027,969</td>
<td headers="perc_u5_poverty" class="gt_row gt_right">16.5%</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="4">Data from 2022 &amp; 2023 American Community Survey 1-year estimates from the U.S. Census Bureau. Estimates exclude Puerto Rico.</td>
    </tr>
  </tfoot>
  
</table>
</div>
```

:::
:::

::: {.cell}
::: {.cell-output .cell-output-stderr}

```
`summarise()` has grouped output by 'year'. You can override using the
`.groups` argument.
```


:::

::: {.cell-output-display}

```{=html}
<div id="mdlfvpczfo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#mdlfvpczfo table {
  font-family: 'Roboto Condensed', system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#mdlfvpczfo thead, #mdlfvpczfo tbody, #mdlfvpczfo tfoot, #mdlfvpczfo tr, #mdlfvpczfo td, #mdlfvpczfo th {
  border-style: none;
}

#mdlfvpczfo p {
  margin: 0;
  padding: 0;
}

#mdlfvpczfo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: rgba(255, 255, 255, 0);
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: rgba(255, 255, 255, 0);
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#mdlfvpczfo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#mdlfvpczfo .gt_title {
  color: #333333;
  font-size: 24px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mdlfvpczfo .gt_subtitle {
  color: #333333;
  font-size: 16px;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mdlfvpczfo .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mdlfvpczfo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mdlfvpczfo .gt_col_headings {
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: rgba(255, 255, 255, 0);
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mdlfvpczfo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 16px;
  font-weight: bolder;
  text-transform: uppercase;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#mdlfvpczfo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 16px;
  font-weight: bolder;
  text-transform: uppercase;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#mdlfvpczfo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mdlfvpczfo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mdlfvpczfo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#mdlfvpczfo .gt_spanner_row {
  border-bottom-style: hidden;
}

#mdlfvpczfo .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#mdlfvpczfo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#mdlfvpczfo .gt_from_md > :first-child {
  margin-top: 0;
}

#mdlfvpczfo .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mdlfvpczfo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#mdlfvpczfo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#mdlfvpczfo .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#mdlfvpczfo .gt_row_group_first td {
  border-top-width: 2px;
}

#mdlfvpczfo .gt_row_group_first th {
  border-top-width: 2px;
}

#mdlfvpczfo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mdlfvpczfo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mdlfvpczfo .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mdlfvpczfo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mdlfvpczfo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mdlfvpczfo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mdlfvpczfo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#mdlfvpczfo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mdlfvpczfo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mdlfvpczfo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mdlfvpczfo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mdlfvpczfo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mdlfvpczfo .gt_sourcenote {
  font-size: 12px;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mdlfvpczfo .gt_left {
  text-align: left;
}

#mdlfvpczfo .gt_center {
  text-align: center;
}

#mdlfvpczfo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mdlfvpczfo .gt_font_normal {
  font-weight: normal;
}

#mdlfvpczfo .gt_font_bold {
  font-weight: bold;
}

#mdlfvpczfo .gt_font_italic {
  font-style: italic;
}

#mdlfvpczfo .gt_super {
  font-size: 65%;
}

#mdlfvpczfo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#mdlfvpczfo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mdlfvpczfo .gt_indent_1 {
  text-indent: 5px;
}

#mdlfvpczfo .gt_indent_2 {
  text-indent: 10px;
}

#mdlfvpczfo .gt_indent_3 {
  text-indent: 15px;
}

#mdlfvpczfo .gt_indent_4 {
  text-indent: 20px;
}

#mdlfvpczfo .gt_indent_5 {
  text-indent: 25px;
}

#mdlfvpczfo .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#mdlfvpczfo div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal" style="font-size: 24; font-weight: bold;"><div data-qmd-base64="KipFc3RpbWF0ZWQgY2hpbGQgcG92ZXJ0eSBpbiB0aGUgVS5TLiAoMjAyMi0yMDIzKSoq"><div class='gt_from_md'><p><strong>Estimated child poverty in the U.S. (2022-2023)</strong></p>
</div></div></td>
    </tr>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Total and Percent estimates of those living below the Federal Poverty Line</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 3px; border-bottom-style: solid; border-bottom-color: #000000; font-weight: bold;" scope="col" id="Year">Year</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 3px; border-bottom-style: solid; border-bottom-color: #000000; font-weight: bold;" scope="col" id="Total children &amp;lt; 5 y.o.">Total children &lt; 5 y.o.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 3px; border-bottom-style: solid; border-bottom-color: #000000; font-weight: bold;" scope="col" id="Total children &amp;lt; 5 y.o. living in poverty in last 12 mos.">Total children &lt; 5 y.o. living in poverty in last 12 mos.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 3px; border-bottom-style: solid; border-bottom-color: #000000; font-weight: bold;" scope="col" id="% of children &amp;lt; 5 y.o. living in poverty in last 12 mos.">% of children &lt; 5 y.o. living in poverty in last 12 mos.</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Alabama">Alabama</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Alabama  year" class="gt_row gt_right">2022</td>
<td headers="Alabama  total_u5_popE" class="gt_row gt_right">284,064</td>
<td headers="Alabama  total_u5_povertyE" class="gt_row gt_right">71,573</td>
<td headers="Alabama  perc_u5_poverty" class="gt_row gt_right" style="background-color: #FCA80D; color: #000000;">25.2%</td></tr>
    <tr><td headers="Alabama  year" class="gt_row gt_right">2023</td>
<td headers="Alabama  total_u5_popE" class="gt_row gt_right">288,019</td>
<td headers="Alabama  total_u5_povertyE" class="gt_row gt_right">66,642</td>
<td headers="Alabama  perc_u5_poverty" class="gt_row gt_right" style="background-color: #F67D15; color: #FFFFFF;">23.1%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Alaska">Alaska</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Alaska  year" class="gt_row gt_right">2022</td>
<td headers="Alaska  total_u5_popE" class="gt_row gt_right">46,497</td>
<td headers="Alaska  total_u5_povertyE" class="gt_row gt_right">9,199</td>
<td headers="Alaska  perc_u5_poverty" class="gt_row gt_right" style="background-color: #D24644; color: #FFFFFF;">19.8%</td></tr>
    <tr><td headers="Alaska  year" class="gt_row gt_right">2023</td>
<td headers="Alaska  total_u5_popE" class="gt_row gt_right">45,211</td>
<td headers="Alaska  total_u5_povertyE" class="gt_row gt_right">6,951</td>
<td headers="Alaska  perc_u5_poverty" class="gt_row gt_right" style="background-color: #86216B; color: #FFFFFF;">15.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Arizona">Arizona</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Arizona  year" class="gt_row gt_right">2022</td>
<td headers="Arizona  total_u5_popE" class="gt_row gt_right">393,413</td>
<td headers="Arizona  total_u5_povertyE" class="gt_row gt_right">68,494</td>
<td headers="Arizona  perc_u5_poverty" class="gt_row gt_right" style="background-color: #AA2F5D; color: #FFFFFF;">17.4%</td></tr>
    <tr><td headers="Arizona  year" class="gt_row gt_right">2023</td>
<td headers="Arizona  total_u5_popE" class="gt_row gt_right">391,142</td>
<td headers="Arizona  total_u5_povertyE" class="gt_row gt_right">62,009</td>
<td headers="Arizona  perc_u5_poverty" class="gt_row gt_right" style="background-color: #8E2469; color: #FFFFFF;">15.9%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Arkansas">Arkansas</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Arkansas  year" class="gt_row gt_right">2022</td>
<td headers="Arkansas  total_u5_popE" class="gt_row gt_right">177,765</td>
<td headers="Arkansas  total_u5_povertyE" class="gt_row gt_right">45,650</td>
<td headers="Arkansas  perc_u5_poverty" class="gt_row gt_right" style="background-color: #FCB317; color: #000000;">25.7%</td></tr>
    <tr><td headers="Arkansas  year" class="gt_row gt_right">2023</td>
<td headers="Arkansas  total_u5_popE" class="gt_row gt_right">176,908</td>
<td headers="Arkansas  total_u5_povertyE" class="gt_row gt_right">39,571</td>
<td headers="Arkansas  perc_u5_poverty" class="gt_row gt_right" style="background-color: #F06F21; color: #FFFFFF;">22.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="California">California</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="California  year" class="gt_row gt_right">2022</td>
<td headers="California  total_u5_popE" class="gt_row gt_right">2,118,386</td>
<td headers="California  total_u5_povertyE" class="gt_row gt_right">320,754</td>
<td headers="California  perc_u5_poverty" class="gt_row gt_right" style="background-color: #811F6C; color: #FFFFFF;">15.1%</td></tr>
    <tr><td headers="California  year" class="gt_row gt_right">2023</td>
<td headers="California  total_u5_popE" class="gt_row gt_right">2,086,820</td>
<td headers="California  total_u5_povertyE" class="gt_row gt_right">298,927</td>
<td headers="California  perc_u5_poverty" class="gt_row gt_right" style="background-color: #72196E; color: #FFFFFF;">14.3%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Colorado">Colorado</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Colorado  year" class="gt_row gt_right">2022</td>
<td headers="Colorado  total_u5_popE" class="gt_row gt_right">305,063</td>
<td headers="Colorado  total_u5_povertyE" class="gt_row gt_right">33,128</td>
<td headers="Colorado  perc_u5_poverty" class="gt_row gt_right" style="background-color: #310A5C; color: #FFFFFF;">10.9%</td></tr>
    <tr><td headers="Colorado  year" class="gt_row gt_right">2023</td>
<td headers="Colorado  total_u5_popE" class="gt_row gt_right">303,775</td>
<td headers="Colorado  total_u5_povertyE" class="gt_row gt_right">32,175</td>
<td headers="Colorado  perc_u5_poverty" class="gt_row gt_right" style="background-color: #2B0B57; color: #FFFFFF;">10.6%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Connecticut">Connecticut</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Connecticut  year" class="gt_row gt_right">2022</td>
<td headers="Connecticut  total_u5_popE" class="gt_row gt_right">178,453</td>
<td headers="Connecticut  total_u5_povertyE" class="gt_row gt_right">23,943</td>
<td headers="Connecticut  perc_u5_poverty" class="gt_row gt_right" style="background-color: #62146E; color: #FFFFFF;">13.4%</td></tr>
    <tr><td headers="Connecticut  year" class="gt_row gt_right">2023</td>
<td headers="Connecticut  total_u5_popE" class="gt_row gt_right">180,561</td>
<td headers="Connecticut  total_u5_povertyE" class="gt_row gt_right">23,506</td>
<td headers="Connecticut  perc_u5_poverty" class="gt_row gt_right" style="background-color: #5B116E; color: #FFFFFF;">13.0%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Delaware">Delaware</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Delaware  year" class="gt_row gt_right">2022</td>
<td headers="Delaware  total_u5_popE" class="gt_row gt_right">54,058</td>
<td headers="Delaware  total_u5_povertyE" class="gt_row gt_right">7,072</td>
<td headers="Delaware  perc_u5_poverty" class="gt_row gt_right" style="background-color: #5C126E; color: #FFFFFF;">13.1%</td></tr>
    <tr><td headers="Delaware  year" class="gt_row gt_right">2023</td>
<td headers="Delaware  total_u5_popE" class="gt_row gt_right">54,398</td>
<td headers="Delaware  total_u5_povertyE" class="gt_row gt_right">8,202</td>
<td headers="Delaware  perc_u5_poverty" class="gt_row gt_right" style="background-color: #801F6C; color: #FFFFFF;">15.1%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="District of Columbia">District of Columbia</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="District of Columbia  year" class="gt_row gt_right">2022</td>
<td headers="District of Columbia  total_u5_popE" class="gt_row gt_right">39,035</td>
<td headers="District of Columbia  total_u5_povertyE" class="gt_row gt_right">5,729</td>
<td headers="District of Columbia  perc_u5_poverty" class="gt_row gt_right" style="background-color: #781C6D; color: #FFFFFF;">14.7%</td></tr>
    <tr><td headers="District of Columbia  year" class="gt_row gt_right">2023</td>
<td headers="District of Columbia  total_u5_popE" class="gt_row gt_right">38,512</td>
<td headers="District of Columbia  total_u5_povertyE" class="gt_row gt_right">5,011</td>
<td headers="District of Columbia  perc_u5_poverty" class="gt_row gt_right" style="background-color: #5B116E; color: #FFFFFF;">13.0%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Florida">Florida</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Florida  year" class="gt_row gt_right">2022</td>
<td headers="Florida  total_u5_popE" class="gt_row gt_right">1,101,350</td>
<td headers="Florida  total_u5_povertyE" class="gt_row gt_right">210,146</td>
<td headers="Florida  perc_u5_poverty" class="gt_row gt_right" style="background-color: #C63E4C; color: #FFFFFF;">19.1%</td></tr>
    <tr><td headers="Florida  year" class="gt_row gt_right">2023</td>
<td headers="Florida  total_u5_popE" class="gt_row gt_right">1,122,270</td>
<td headers="Florida  total_u5_povertyE" class="gt_row gt_right">183,922</td>
<td headers="Florida  perc_u5_poverty" class="gt_row gt_right" style="background-color: #982766; color: #FFFFFF;">16.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Georgia">Georgia</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Georgia  year" class="gt_row gt_right">2022</td>
<td headers="Georgia  total_u5_popE" class="gt_row gt_right">621,126</td>
<td headers="Georgia  total_u5_povertyE" class="gt_row gt_right">110,286</td>
<td headers="Georgia  perc_u5_poverty" class="gt_row gt_right" style="background-color: #B0325A; color: #FFFFFF;">17.8%</td></tr>
    <tr><td headers="Georgia  year" class="gt_row gt_right">2023</td>
<td headers="Georgia  total_u5_popE" class="gt_row gt_right">621,750</td>
<td headers="Georgia  total_u5_povertyE" class="gt_row gt_right">118,763</td>
<td headers="Georgia  perc_u5_poverty" class="gt_row gt_right" style="background-color: #C63E4C; color: #FFFFFF;">19.1%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Hawaii">Hawaii</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Hawaii  year" class="gt_row gt_right">2022</td>
<td headers="Hawaii  total_u5_popE" class="gt_row gt_right">78,927</td>
<td headers="Hawaii  total_u5_povertyE" class="gt_row gt_right">9,405</td>
<td headers="Hawaii  perc_u5_poverty" class="gt_row gt_right" style="background-color: #460B69; color: #FFFFFF;">11.9%</td></tr>
    <tr><td headers="Hawaii  year" class="gt_row gt_right">2023</td>
<td headers="Hawaii  total_u5_popE" class="gt_row gt_right">77,420</td>
<td headers="Hawaii  total_u5_povertyE" class="gt_row gt_right">8,020</td>
<td headers="Hawaii  perc_u5_poverty" class="gt_row gt_right" style="background-color: #270B52; color: #FFFFFF;">10.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Idaho">Idaho</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Idaho  year" class="gt_row gt_right">2022</td>
<td headers="Idaho  total_u5_popE" class="gt_row gt_right">111,816</td>
<td headers="Idaho  total_u5_povertyE" class="gt_row gt_right">15,600</td>
<td headers="Idaho  perc_u5_poverty" class="gt_row gt_right" style="background-color: #6B186E; color: #FFFFFF;">14.0%</td></tr>
    <tr><td headers="Idaho  year" class="gt_row gt_right">2023</td>
<td headers="Idaho  total_u5_popE" class="gt_row gt_right">110,908</td>
<td headers="Idaho  total_u5_povertyE" class="gt_row gt_right">15,449</td>
<td headers="Idaho  perc_u5_poverty" class="gt_row gt_right" style="background-color: #6B176E; color: #FFFFFF;">13.9%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Illinois">Illinois</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Illinois  year" class="gt_row gt_right">2022</td>
<td headers="Illinois  total_u5_popE" class="gt_row gt_right">674,211</td>
<td headers="Illinois  total_u5_povertyE" class="gt_row gt_right">111,480</td>
<td headers="Illinois  perc_u5_poverty" class="gt_row gt_right" style="background-color: #9A2865; color: #FFFFFF;">16.5%</td></tr>
    <tr><td headers="Illinois  year" class="gt_row gt_right">2023</td>
<td headers="Illinois  total_u5_popE" class="gt_row gt_right">661,026</td>
<td headers="Illinois  total_u5_povertyE" class="gt_row gt_right">97,150</td>
<td headers="Illinois  perc_u5_poverty" class="gt_row gt_right" style="background-color: #791C6D; color: #FFFFFF;">14.7%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Indiana">Indiana</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Indiana  year" class="gt_row gt_right">2022</td>
<td headers="Indiana  total_u5_popE" class="gt_row gt_right">399,031</td>
<td headers="Indiana  total_u5_povertyE" class="gt_row gt_right">71,339</td>
<td headers="Indiana  perc_u5_poverty" class="gt_row gt_right" style="background-color: #B23359; color: #FFFFFF;">17.9%</td></tr>
    <tr><td headers="Indiana  year" class="gt_row gt_right">2023</td>
<td headers="Indiana  total_u5_popE" class="gt_row gt_right">401,558</td>
<td headers="Indiana  total_u5_povertyE" class="gt_row gt_right">67,570</td>
<td headers="Indiana  perc_u5_poverty" class="gt_row gt_right" style="background-color: #A02A63; color: #FFFFFF;">16.8%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Iowa">Iowa</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Iowa  year" class="gt_row gt_right">2022</td>
<td headers="Iowa  total_u5_popE" class="gt_row gt_right">180,010</td>
<td headers="Iowa  total_u5_povertyE" class="gt_row gt_right">24,655</td>
<td headers="Iowa  perc_u5_poverty" class="gt_row gt_right" style="background-color: #67166E; color: #FFFFFF;">13.7%</td></tr>
    <tr><td headers="Iowa  year" class="gt_row gt_right">2023</td>
<td headers="Iowa  total_u5_popE" class="gt_row gt_right">182,063</td>
<td headers="Iowa  total_u5_povertyE" class="gt_row gt_right">27,260</td>
<td headers="Iowa  perc_u5_poverty" class="gt_row gt_right" style="background-color: #7E1E6D; color: #FFFFFF;">15.0%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Kansas">Kansas</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Kansas  year" class="gt_row gt_right">2022</td>
<td headers="Kansas  total_u5_popE" class="gt_row gt_right">176,673</td>
<td headers="Kansas  total_u5_povertyE" class="gt_row gt_right">26,764</td>
<td headers="Kansas  perc_u5_poverty" class="gt_row gt_right" style="background-color: #811F6C; color: #FFFFFF;">15.1%</td></tr>
    <tr><td headers="Kansas  year" class="gt_row gt_right">2023</td>
<td headers="Kansas  total_u5_popE" class="gt_row gt_right">169,830</td>
<td headers="Kansas  total_u5_povertyE" class="gt_row gt_right">24,238</td>
<td headers="Kansas  perc_u5_poverty" class="gt_row gt_right" style="background-color: #72196E; color: #FFFFFF;">14.3%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Kentucky">Kentucky</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Kentucky  year" class="gt_row gt_right">2022</td>
<td headers="Kentucky  total_u5_popE" class="gt_row gt_right">260,433</td>
<td headers="Kentucky  total_u5_povertyE" class="gt_row gt_right">58,904</td>
<td headers="Kentucky  perc_u5_poverty" class="gt_row gt_right" style="background-color: #F2731D; color: #FFFFFF;">22.6%</td></tr>
    <tr><td headers="Kentucky  year" class="gt_row gt_right">2023</td>
<td headers="Kentucky  total_u5_popE" class="gt_row gt_right">264,633</td>
<td headers="Kentucky  total_u5_povertyE" class="gt_row gt_right">60,196</td>
<td headers="Kentucky  perc_u5_poverty" class="gt_row gt_right" style="background-color: #F3751B; color: #FFFFFF;">22.7%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Louisiana">Louisiana</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Louisiana  year" class="gt_row gt_right">2022</td>
<td headers="Louisiana  total_u5_popE" class="gt_row gt_right">270,937</td>
<td headers="Louisiana  total_u5_povertyE" class="gt_row gt_right">67,924</td>
<td headers="Louisiana  perc_u5_poverty" class="gt_row gt_right" style="background-color: #FCA60B; color: #000000;">25.1%</td></tr>
    <tr><td headers="Louisiana  year" class="gt_row gt_right">2023</td>
<td headers="Louisiana  total_u5_popE" class="gt_row gt_right">275,636</td>
<td headers="Louisiana  total_u5_povertyE" class="gt_row gt_right">72,151</td>
<td headers="Louisiana  perc_u5_poverty" class="gt_row gt_right" style="background-color: #FBBF25; color: #000000;">26.2%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Maine">Maine</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Maine  year" class="gt_row gt_right">2022</td>
<td headers="Maine  total_u5_popE" class="gt_row gt_right">61,018</td>
<td headers="Maine  total_u5_povertyE" class="gt_row gt_right">6,750</td>
<td headers="Maine  perc_u5_poverty" class="gt_row gt_right" style="background-color: #360961; color: #FFFFFF;">11.1%</td></tr>
    <tr><td headers="Maine  year" class="gt_row gt_right">2023</td>
<td headers="Maine  total_u5_popE" class="gt_row gt_right">59,898</td>
<td headers="Maine  total_u5_povertyE" class="gt_row gt_right">6,982</td>
<td headers="Maine  perc_u5_poverty" class="gt_row gt_right" style="background-color: #410A67; color: #FFFFFF;">11.7%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Maryland">Maryland</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Maryland  year" class="gt_row gt_right">2022</td>
<td headers="Maryland  total_u5_popE" class="gt_row gt_right">349,193</td>
<td headers="Maryland  total_u5_povertyE" class="gt_row gt_right">39,601</td>
<td headers="Maryland  perc_u5_poverty" class="gt_row gt_right" style="background-color: #3B0964; color: #FFFFFF;">11.3%</td></tr>
    <tr><td headers="Maryland  year" class="gt_row gt_right">2023</td>
<td headers="Maryland  total_u5_popE" class="gt_row gt_right">346,836</td>
<td headers="Maryland  total_u5_povertyE" class="gt_row gt_right">41,543</td>
<td headers="Maryland  perc_u5_poverty" class="gt_row gt_right" style="background-color: #470B6A; color: #FFFFFF;">12.0%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Massachusetts">Massachusetts</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Massachusetts  year" class="gt_row gt_right">2022</td>
<td headers="Massachusetts  total_u5_popE" class="gt_row gt_right">342,252</td>
<td headers="Massachusetts  total_u5_povertyE" class="gt_row gt_right">36,145</td>
<td headers="Massachusetts  perc_u5_poverty" class="gt_row gt_right" style="background-color: #2B0B56; color: #FFFFFF;">10.6%</td></tr>
    <tr><td headers="Massachusetts  year" class="gt_row gt_right">2023</td>
<td headers="Massachusetts  total_u5_popE" class="gt_row gt_right">342,145</td>
<td headers="Massachusetts  total_u5_povertyE" class="gt_row gt_right">41,153</td>
<td headers="Massachusetts  perc_u5_poverty" class="gt_row gt_right" style="background-color: #480B6A; color: #FFFFFF;">12.0%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Michigan">Michigan</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Michigan  year" class="gt_row gt_right">2022</td>
<td headers="Michigan  total_u5_popE" class="gt_row gt_right">536,805</td>
<td headers="Michigan  total_u5_povertyE" class="gt_row gt_right">105,043</td>
<td headers="Michigan  perc_u5_poverty" class="gt_row gt_right" style="background-color: #CF4446; color: #FFFFFF;">19.6%</td></tr>
    <tr><td headers="Michigan  year" class="gt_row gt_right">2023</td>
<td headers="Michigan  total_u5_popE" class="gt_row gt_right">529,459</td>
<td headers="Michigan  total_u5_povertyE" class="gt_row gt_right">93,760</td>
<td headers="Michigan  perc_u5_poverty" class="gt_row gt_right" style="background-color: #AF315B; color: #FFFFFF;">17.7%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Minnesota">Minnesota</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Minnesota  year" class="gt_row gt_right">2022</td>
<td headers="Minnesota  total_u5_popE" class="gt_row gt_right">328,095</td>
<td headers="Minnesota  total_u5_povertyE" class="gt_row gt_right">37,251</td>
<td headers="Minnesota  perc_u5_poverty" class="gt_row gt_right" style="background-color: #3B0964; color: #FFFFFF;">11.4%</td></tr>
    <tr><td headers="Minnesota  year" class="gt_row gt_right">2023</td>
<td headers="Minnesota  total_u5_popE" class="gt_row gt_right">326,995</td>
<td headers="Minnesota  total_u5_povertyE" class="gt_row gt_right">33,516</td>
<td headers="Minnesota  perc_u5_poverty" class="gt_row gt_right" style="background-color: #240C4F; color: #FFFFFF;">10.2%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Mississippi">Mississippi</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Mississippi  year" class="gt_row gt_right">2022</td>
<td headers="Mississippi  total_u5_popE" class="gt_row gt_right">169,303</td>
<td headers="Mississippi  total_u5_povertyE" class="gt_row gt_right">42,197</td>
<td headers="Mississippi  perc_u5_poverty" class="gt_row gt_right" style="background-color: #FCA309; color: #000000;">24.9%</td></tr>
    <tr><td headers="Mississippi  year" class="gt_row gt_right">2023</td>
<td headers="Mississippi  total_u5_popE" class="gt_row gt_right">167,015</td>
<td headers="Mississippi  total_u5_povertyE" class="gt_row gt_right">37,854</td>
<td headers="Mississippi  perc_u5_poverty" class="gt_row gt_right" style="background-color: #F2741C; color: #FFFFFF;">22.7%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Missouri">Missouri</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Missouri  year" class="gt_row gt_right">2022</td>
<td headers="Missouri  total_u5_popE" class="gt_row gt_right">349,648</td>
<td headers="Missouri  total_u5_povertyE" class="gt_row gt_right">64,640</td>
<td headers="Missouri  perc_u5_poverty" class="gt_row gt_right" style="background-color: #BD3853; color: #FFFFFF;">18.5%</td></tr>
    <tr><td headers="Missouri  year" class="gt_row gt_right">2023</td>
<td headers="Missouri  total_u5_popE" class="gt_row gt_right">348,416</td>
<td headers="Missouri  total_u5_povertyE" class="gt_row gt_right">54,655</td>
<td headers="Missouri  perc_u5_poverty" class="gt_row gt_right" style="background-color: #8B2369; color: #FFFFFF;">15.7%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Montana">Montana</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Montana  year" class="gt_row gt_right">2022</td>
<td headers="Montana  total_u5_popE" class="gt_row gt_right">57,024</td>
<td headers="Montana  total_u5_povertyE" class="gt_row gt_right">7,363</td>
<td headers="Montana  perc_u5_poverty" class="gt_row gt_right" style="background-color: #59106E; color: #FFFFFF;">12.9%</td></tr>
    <tr><td headers="Montana  year" class="gt_row gt_right">2023</td>
<td headers="Montana  total_u5_popE" class="gt_row gt_right">55,363</td>
<td headers="Montana  total_u5_povertyE" class="gt_row gt_right">6,976</td>
<td headers="Montana  perc_u5_poverty" class="gt_row gt_right" style="background-color: #530F6D; color: #FFFFFF;">12.6%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Nebraska">Nebraska</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Nebraska  year" class="gt_row gt_right">2022</td>
<td headers="Nebraska  total_u5_popE" class="gt_row gt_right">121,107</td>
<td headers="Nebraska  total_u5_povertyE" class="gt_row gt_right">17,323</td>
<td headers="Nebraska  perc_u5_poverty" class="gt_row gt_right" style="background-color: #72196E; color: #FFFFFF;">14.3%</td></tr>
    <tr><td headers="Nebraska  year" class="gt_row gt_right">2023</td>
<td headers="Nebraska  total_u5_popE" class="gt_row gt_right">120,499</td>
<td headers="Nebraska  total_u5_povertyE" class="gt_row gt_right">14,977</td>
<td headers="Nebraska  perc_u5_poverty" class="gt_row gt_right" style="background-color: #500D6C; color: #FFFFFF;">12.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Nevada">Nevada</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Nevada  year" class="gt_row gt_right">2022</td>
<td headers="Nevada  total_u5_popE" class="gt_row gt_right">172,575</td>
<td headers="Nevada  total_u5_povertyE" class="gt_row gt_right">29,294</td>
<td headers="Nevada  perc_u5_poverty" class="gt_row gt_right" style="background-color: #A32B61; color: #FFFFFF;">17.0%</td></tr>
    <tr><td headers="Nevada  year" class="gt_row gt_right">2023</td>
<td headers="Nevada  total_u5_popE" class="gt_row gt_right">171,163</td>
<td headers="Nevada  total_u5_povertyE" class="gt_row gt_right">33,530</td>
<td headers="Nevada  perc_u5_poverty" class="gt_row gt_right" style="background-color: #CF4446; color: #FFFFFF;">19.6%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="New Hampshire">New Hampshire</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="New Hampshire  year" class="gt_row gt_right">2022</td>
<td headers="New Hampshire  total_u5_popE" class="gt_row gt_right">62,666</td>
<td headers="New Hampshire  total_u5_povertyE" class="gt_row gt_right">5,192</td>
<td headers="New Hampshire  perc_u5_poverty" class="gt_row gt_right" style="background-color: #07051B; color: #FFFFFF;">8.3%</td></tr>
    <tr><td headers="New Hampshire  year" class="gt_row gt_right">2023</td>
<td headers="New Hampshire  total_u5_popE" class="gt_row gt_right">62,779</td>
<td headers="New Hampshire  total_u5_povertyE" class="gt_row gt_right">4,565</td>
<td headers="New Hampshire  perc_u5_poverty" class="gt_row gt_right" style="background-color: #000004; color: #FFFFFF;">7.3%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="New Jersey">New Jersey</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="New Jersey  year" class="gt_row gt_right">2022</td>
<td headers="New Jersey  total_u5_popE" class="gt_row gt_right">513,333</td>
<td headers="New Jersey  total_u5_povertyE" class="gt_row gt_right">68,408</td>
<td headers="New Jersey  perc_u5_poverty" class="gt_row gt_right" style="background-color: #60136E; color: #FFFFFF;">13.3%</td></tr>
    <tr><td headers="New Jersey  year" class="gt_row gt_right">2023</td>
<td headers="New Jersey  total_u5_popE" class="gt_row gt_right">518,528</td>
<td headers="New Jersey  total_u5_povertyE" class="gt_row gt_right">64,651</td>
<td headers="New Jersey  perc_u5_poverty" class="gt_row gt_right" style="background-color: #510D6C; color: #FFFFFF;">12.5%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="New Mexico">New Mexico</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="New Mexico  year" class="gt_row gt_right">2022</td>
<td headers="New Mexico  total_u5_popE" class="gt_row gt_right">104,994</td>
<td headers="New Mexico  total_u5_povertyE" class="gt_row gt_right">23,304</td>
<td headers="New Mexico  perc_u5_poverty" class="gt_row gt_right" style="background-color: #EE6C23; color: #FFFFFF;">22.2%</td></tr>
    <tr><td headers="New Mexico  year" class="gt_row gt_right">2023</td>
<td headers="New Mexico  total_u5_popE" class="gt_row gt_right">104,293</td>
<td headers="New Mexico  total_u5_povertyE" class="gt_row gt_right">28,040</td>
<td headers="New Mexico  perc_u5_poverty" class="gt_row gt_right" style="background-color: #F8CE39; color: #000000;">26.9%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="New York">New York</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="New York  year" class="gt_row gt_right">2022</td>
<td headers="New York  total_u5_popE" class="gt_row gt_right">1,055,455</td>
<td headers="New York  total_u5_povertyE" class="gt_row gt_right">191,446</td>
<td headers="New York  perc_u5_poverty" class="gt_row gt_right" style="background-color: #B73557; color: #FFFFFF;">18.1%</td></tr>
    <tr><td headers="New York  year" class="gt_row gt_right">2023</td>
<td headers="New York  total_u5_popE" class="gt_row gt_right">1,035,708</td>
<td headers="New York  total_u5_povertyE" class="gt_row gt_right">191,550</td>
<td headers="New York  perc_u5_poverty" class="gt_row gt_right" style="background-color: #BD3853; color: #FFFFFF;">18.5%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="North Carolina">North Carolina</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="North Carolina  year" class="gt_row gt_right">2022</td>
<td headers="North Carolina  total_u5_popE" class="gt_row gt_right">584,492</td>
<td headers="North Carolina  total_u5_povertyE" class="gt_row gt_right">105,845</td>
<td headers="North Carolina  perc_u5_poverty" class="gt_row gt_right" style="background-color: #B73457; color: #FFFFFF;">18.1%</td></tr>
    <tr><td headers="North Carolina  year" class="gt_row gt_right">2023</td>
<td headers="North Carolina  total_u5_popE" class="gt_row gt_right">594,739</td>
<td headers="North Carolina  total_u5_povertyE" class="gt_row gt_right">107,284</td>
<td headers="North Carolina  perc_u5_poverty" class="gt_row gt_right" style="background-color: #B53458; color: #FFFFFF;">18.0%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="North Dakota">North Dakota</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="North Dakota  year" class="gt_row gt_right">2022</td>
<td headers="North Dakota  total_u5_popE" class="gt_row gt_right">47,844</td>
<td headers="North Dakota  total_u5_povertyE" class="gt_row gt_right">5,553</td>
<td headers="North Dakota  perc_u5_poverty" class="gt_row gt_right" style="background-color: #400967; color: #FFFFFF;">11.6%</td></tr>
    <tr><td headers="North Dakota  year" class="gt_row gt_right">2023</td>
<td headers="North Dakota  total_u5_popE" class="gt_row gt_right">46,488</td>
<td headers="North Dakota  total_u5_povertyE" class="gt_row gt_right">3,575</td>
<td headers="North Dakota  perc_u5_poverty" class="gt_row gt_right" style="background-color: #02020B; color: #FFFFFF;">7.7%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Ohio">Ohio</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Ohio  year" class="gt_row gt_right">2022</td>
<td headers="Ohio  total_u5_popE" class="gt_row gt_right">661,196</td>
<td headers="Ohio  total_u5_povertyE" class="gt_row gt_right">123,512</td>
<td headers="Ohio  perc_u5_poverty" class="gt_row gt_right" style="background-color: #C03A51; color: #FFFFFF;">18.7%</td></tr>
    <tr><td headers="Ohio  year" class="gt_row gt_right">2023</td>
<td headers="Ohio  total_u5_popE" class="gt_row gt_right">654,683</td>
<td headers="Ohio  total_u5_povertyE" class="gt_row gt_right">121,143</td>
<td headers="Ohio  perc_u5_poverty" class="gt_row gt_right" style="background-color: #BD3853; color: #FFFFFF;">18.5%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Oklahoma">Oklahoma</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Oklahoma  year" class="gt_row gt_right">2022</td>
<td headers="Oklahoma  total_u5_popE" class="gt_row gt_right">240,173</td>
<td headers="Oklahoma  total_u5_povertyE" class="gt_row gt_right">50,620</td>
<td headers="Oklahoma  perc_u5_poverty" class="gt_row gt_right" style="background-color: #E35933; color: #FFFFFF;">21.1%</td></tr>
    <tr><td headers="Oklahoma  year" class="gt_row gt_right">2023</td>
<td headers="Oklahoma  total_u5_popE" class="gt_row gt_right">239,611</td>
<td headers="Oklahoma  total_u5_povertyE" class="gt_row gt_right">54,783</td>
<td headers="Oklahoma  perc_u5_poverty" class="gt_row gt_right" style="background-color: #F47719; color: #FFFFFF;">22.9%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Oregon">Oregon</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Oregon  year" class="gt_row gt_right">2022</td>
<td headers="Oregon  total_u5_popE" class="gt_row gt_right">199,584</td>
<td headers="Oregon  total_u5_povertyE" class="gt_row gt_right">29,443</td>
<td headers="Oregon  perc_u5_poverty" class="gt_row gt_right" style="background-color: #7A1D6D; color: #FFFFFF;">14.8%</td></tr>
    <tr><td headers="Oregon  year" class="gt_row gt_right">2023</td>
<td headers="Oregon  total_u5_popE" class="gt_row gt_right">198,150</td>
<td headers="Oregon  total_u5_povertyE" class="gt_row gt_right">28,553</td>
<td headers="Oregon  perc_u5_poverty" class="gt_row gt_right" style="background-color: #741A6E; color: #FFFFFF;">14.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Pennsylvania">Pennsylvania</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Pennsylvania  year" class="gt_row gt_right">2022</td>
<td headers="Pennsylvania  total_u5_popE" class="gt_row gt_right">668,734</td>
<td headers="Pennsylvania  total_u5_povertyE" class="gt_row gt_right">106,428</td>
<td headers="Pennsylvania  perc_u5_poverty" class="gt_row gt_right" style="background-color: #8F2468; color: #FFFFFF;">15.9%</td></tr>
    <tr><td headers="Pennsylvania  year" class="gt_row gt_right">2023</td>
<td headers="Pennsylvania  total_u5_popE" class="gt_row gt_right">663,339</td>
<td headers="Pennsylvania  total_u5_povertyE" class="gt_row gt_right">111,980</td>
<td headers="Pennsylvania  perc_u5_poverty" class="gt_row gt_right" style="background-color: #A22B62; color: #FFFFFF;">16.9%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Rhode Island">Rhode Island</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Rhode Island  year" class="gt_row gt_right">2022</td>
<td headers="Rhode Island  total_u5_popE" class="gt_row gt_right">51,955</td>
<td headers="Rhode Island  total_u5_povertyE" class="gt_row gt_right">5,117</td>
<td headers="Rhode Island  perc_u5_poverty" class="gt_row gt_right" style="background-color: #1D0C45; color: #FFFFFF;">9.8%</td></tr>
    <tr><td headers="Rhode Island  year" class="gt_row gt_right">2023</td>
<td headers="Rhode Island  total_u5_popE" class="gt_row gt_right">52,718</td>
<td headers="Rhode Island  total_u5_povertyE" class="gt_row gt_right">7,062</td>
<td headers="Rhode Island  perc_u5_poverty" class="gt_row gt_right" style="background-color: #61146E; color: #FFFFFF;">13.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="South Carolina">South Carolina</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="South Carolina  year" class="gt_row gt_right">2022</td>
<td headers="South Carolina  total_u5_popE" class="gt_row gt_right">281,426</td>
<td headers="South Carolina  total_u5_povertyE" class="gt_row gt_right">56,398</td>
<td headers="South Carolina  perc_u5_poverty" class="gt_row gt_right" style="background-color: #D54A41; color: #FFFFFF;">20.0%</td></tr>
    <tr><td headers="South Carolina  year" class="gt_row gt_right">2023</td>
<td headers="South Carolina  total_u5_popE" class="gt_row gt_right">285,830</td>
<td headers="South Carolina  total_u5_povertyE" class="gt_row gt_right">55,388</td>
<td headers="South Carolina  perc_u5_poverty" class="gt_row gt_right" style="background-color: #CB4149; color: #FFFFFF;">19.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="South Dakota">South Dakota</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="South Dakota  year" class="gt_row gt_right">2022</td>
<td headers="South Dakota  total_u5_popE" class="gt_row gt_right">57,246</td>
<td headers="South Dakota  total_u5_povertyE" class="gt_row gt_right">10,086</td>
<td headers="South Dakota  perc_u5_poverty" class="gt_row gt_right" style="background-color: #AE305C; color: #FFFFFF;">17.6%</td></tr>
    <tr><td headers="South Dakota  year" class="gt_row gt_right">2023</td>
<td headers="South Dakota  total_u5_popE" class="gt_row gt_right">54,886</td>
<td headers="South Dakota  total_u5_povertyE" class="gt_row gt_right">8,240</td>
<td headers="South Dakota  perc_u5_poverty" class="gt_row gt_right" style="background-color: #7F1F6C; color: #FFFFFF;">15.0%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Tennessee">Tennessee</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Tennessee  year" class="gt_row gt_right">2022</td>
<td headers="Tennessee  total_u5_popE" class="gt_row gt_right">402,215</td>
<td headers="Tennessee  total_u5_povertyE" class="gt_row gt_right">78,962</td>
<td headers="Tennessee  perc_u5_poverty" class="gt_row gt_right" style="background-color: #D04446; color: #FFFFFF;">19.6%</td></tr>
    <tr><td headers="Tennessee  year" class="gt_row gt_right">2023</td>
<td headers="Tennessee  total_u5_popE" class="gt_row gt_right">411,032</td>
<td headers="Tennessee  total_u5_povertyE" class="gt_row gt_right">80,230</td>
<td headers="Tennessee  perc_u5_poverty" class="gt_row gt_right" style="background-color: #CE4347; color: #FFFFFF;">19.5%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Texas">Texas</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Texas  year" class="gt_row gt_right">2022</td>
<td headers="Texas  total_u5_popE" class="gt_row gt_right">1,881,718</td>
<td headers="Texas  total_u5_povertyE" class="gt_row gt_right">389,244</td>
<td headers="Texas  perc_u5_poverty" class="gt_row gt_right" style="background-color: #DF5237; color: #FFFFFF;">20.7%</td></tr>
    <tr><td headers="Texas  year" class="gt_row gt_right">2023</td>
<td headers="Texas  total_u5_popE" class="gt_row gt_right">1,913,591</td>
<td headers="Texas  total_u5_povertyE" class="gt_row gt_right">370,590</td>
<td headers="Texas  perc_u5_poverty" class="gt_row gt_right" style="background-color: #CB4149; color: #FFFFFF;">19.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Utah">Utah</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Utah  year" class="gt_row gt_right">2022</td>
<td headers="Utah  total_u5_popE" class="gt_row gt_right">228,464</td>
<td headers="Utah  total_u5_povertyE" class="gt_row gt_right">23,266</td>
<td headers="Utah  perc_u5_poverty" class="gt_row gt_right" style="background-color: #230C4D; color: #FFFFFF;">10.2%</td></tr>
    <tr><td headers="Utah  year" class="gt_row gt_right">2023</td>
<td headers="Utah  total_u5_popE" class="gt_row gt_right">229,881</td>
<td headers="Utah  total_u5_povertyE" class="gt_row gt_right">22,545</td>
<td headers="Utah  perc_u5_poverty" class="gt_row gt_right" style="background-color: #1C0C44; color: #FFFFFF;">9.8%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Vermont">Vermont</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Vermont  year" class="gt_row gt_right">2022</td>
<td headers="Vermont  total_u5_popE" class="gt_row gt_right">27,875</td>
<td headers="Vermont  total_u5_povertyE" class="gt_row gt_right">4,312</td>
<td headers="Vermont  perc_u5_poverty" class="gt_row gt_right" style="background-color: #87226A; color: #FFFFFF;">15.5%</td></tr>
    <tr><td headers="Vermont  year" class="gt_row gt_right">2023</td>
<td headers="Vermont  total_u5_popE" class="gt_row gt_right">27,168</td>
<td headers="Vermont  total_u5_povertyE" class="gt_row gt_right">2,295</td>
<td headers="Vermont  perc_u5_poverty" class="gt_row gt_right" style="background-color: #09051E; color: #FFFFFF;">8.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Virginia">Virginia</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Virginia  year" class="gt_row gt_right">2022</td>
<td headers="Virginia  total_u5_popE" class="gt_row gt_right">481,682</td>
<td headers="Virginia  total_u5_povertyE" class="gt_row gt_right">68,691</td>
<td headers="Virginia  perc_u5_poverty" class="gt_row gt_right" style="background-color: #72196E; color: #FFFFFF;">14.3%</td></tr>
    <tr><td headers="Virginia  year" class="gt_row gt_right">2023</td>
<td headers="Virginia  total_u5_popE" class="gt_row gt_right">476,744</td>
<td headers="Virginia  total_u5_povertyE" class="gt_row gt_right">59,128</td>
<td headers="Virginia  perc_u5_poverty" class="gt_row gt_right" style="background-color: #500D6C; color: #FFFFFF;">12.4%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Washington">Washington</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Washington  year" class="gt_row gt_right">2022</td>
<td headers="Washington  total_u5_popE" class="gt_row gt_right">421,722</td>
<td headers="Washington  total_u5_povertyE" class="gt_row gt_right">44,774</td>
<td headers="Washington  perc_u5_poverty" class="gt_row gt_right" style="background-color: #2C0B58; color: #FFFFFF;">10.6%</td></tr>
    <tr><td headers="Washington  year" class="gt_row gt_right">2023</td>
<td headers="Washington  total_u5_popE" class="gt_row gt_right">417,322</td>
<td headers="Washington  total_u5_povertyE" class="gt_row gt_right">55,639</td>
<td headers="Washington  perc_u5_poverty" class="gt_row gt_right" style="background-color: #60136E; color: #FFFFFF;">13.3%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="West Virginia">West Virginia</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="West Virginia  year" class="gt_row gt_right">2022</td>
<td headers="West Virginia  total_u5_popE" class="gt_row gt_right">87,469</td>
<td headers="West Virginia  total_u5_povertyE" class="gt_row gt_right">25,753</td>
<td headers="West Virginia  perc_u5_poverty" class="gt_row gt_right" style="background-color: #FCFFA4; color: #000000;">29.4%</td></tr>
    <tr><td headers="West Virginia  year" class="gt_row gt_right">2023</td>
<td headers="West Virginia  total_u5_popE" class="gt_row gt_right">87,453</td>
<td headers="West Virginia  total_u5_povertyE" class="gt_row gt_right">21,532</td>
<td headers="West Virginia  perc_u5_poverty" class="gt_row gt_right" style="background-color: #FB9C06; color: #000000;">24.6%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Wisconsin">Wisconsin</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Wisconsin  year" class="gt_row gt_right">2022</td>
<td headers="Wisconsin  total_u5_popE" class="gt_row gt_right">309,244</td>
<td headers="Wisconsin  total_u5_povertyE" class="gt_row gt_right">42,078</td>
<td headers="Wisconsin  perc_u5_poverty" class="gt_row gt_right" style="background-color: #65156E; color: #FFFFFF;">13.6%</td></tr>
    <tr><td headers="Wisconsin  year" class="gt_row gt_right">2023</td>
<td headers="Wisconsin  total_u5_popE" class="gt_row gt_right">307,874</td>
<td headers="Wisconsin  total_u5_povertyE" class="gt_row gt_right">40,988</td>
<td headers="Wisconsin  perc_u5_poverty" class="gt_row gt_right" style="background-color: #60136E; color: #FFFFFF;">13.3%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Wyoming">Wyoming</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Wyoming  year" class="gt_row gt_right">2022</td>
<td headers="Wyoming  total_u5_popE" class="gt_row gt_right">30,444</td>
<td headers="Wyoming  total_u5_povertyE" class="gt_row gt_right">5,023</td>
<td headers="Wyoming  perc_u5_poverty" class="gt_row gt_right" style="background-color: #9A2766; color: #FFFFFF;">16.5%</td></tr>
    <tr><td headers="Wyoming  year" class="gt_row gt_right">2023</td>
<td headers="Wyoming  total_u5_popE" class="gt_row gt_right">30,651</td>
<td headers="Wyoming  total_u5_povertyE" class="gt_row gt_right">5,549</td>
<td headers="Wyoming  perc_u5_poverty" class="gt_row gt_right" style="background-color: #B63457; color: #FFFFFF;">18.1%</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="4">Data from 2022 &amp; 2023 American Community Survey 1-year estimates from the U.S. Census Bureau</td>
    </tr>
  </tfoot>
  
</table>
</div>
```

:::
:::



To further visualize the state-level changes in estimates, I wanted to
try out a **comet plot**. Comet plots are stylized connected dot plots,
which show the "before and after" at both ends of the plot. The comet's
"tail" is the before, and where the estimate moved to is the
comet-shaped end. I've never used one before, but I think that they're
more than just a fun way to plot data. I think that the *movement*
associated with the comet analogy gives the design some intuition.

I was inspired by [this
plot](https://thef5.substack.com/p/how-to-comet-plot), which evaluates
NBA player performance using a comet plot. I borrowed heavily from this
code and aesthetic.



::: {.cell}

```{.r .cell-code}
combined_acs_data %>% 
  filter(year == 2023,
         NAME != 'Puerto Rico') %>% 
  arrange(-perc_diff_YoY) %>% 
  mutate(state = factor(NAME, levels = NAME),
         direction = case_when(
           perc_diff_YoY > .005 ~ "Increase",
           perc_diff_YoY < -.005 ~ "Decrease",
           TRUE ~ "No Change")) %>% 
  ggplot(aes(y = reorder(NAME, -perc_diff_YoY),
             color=direction)) +
  geom_link(aes(x = perc_u5_poverty_tminus_1, y = fct_reorder(NAME, perc_diff_YoY), 
              xend = perc_u5_poverty, yend = fct_reorder(NAME, perc_diff_YoY), 
              size = after_stat(index))) + 
  geom_point(data = . %>% filter(perc_diff_YoY > 0),
             aes(perc_u5_poverty, y = fct_reorder(NAME, perc_diff_YoY)),
             shape = 21, fill = "white", size = 4)  +
  geom_point(data = . %>% filter(perc_diff_YoY < 0),
             aes(perc_u5_poverty, y = fct_reorder(NAME, perc_diff_YoY)),
             shape = 21, fill = "white", size = 4) +
  annotate(geom = 'label', x = .27, y = 49, label = "Poverty rate increased\n in 2023", 
         family = "Roboto Condensed", color = "#E64B35FF", fontface = 'bold', 
         fill = "floralwhite",
         label.size = 0, size = 4) + 
  annotate(geom = 'label', x = .27, y = 36.5, label = "Poverty rate unchanged\n in 2023", 
         family = "Roboto Condensed", color = "#444444", fontface = 'bold', 
         fill = "floralwhite",
         label.size = 0, size = 4) + 
  annotate(geom = 'label', x = .27, y = 24.5, label = "Poverty rate decreased\nin 2023", 
         family = "Roboto Condensed", color = "#00A087FF", fontface = 'bold', 
         fill = "floralwhite",
         label.size = 0, size = 4) + 
  scale_color_manual(values = c("Decrease" = "#00A087FF", "Increase" = "#E64B35FF", "No Change" = "#444444")) +
  scale_size(range = c(.01, 4)) +
  labs(title='Change in State-level Child Poverty<br>from 2022 to 2023',
       subtitle = 'Length of the color band corresponds to the magnitude of change<br>', 
       caption="<br>Source: U.S. Census Bureau's American Community Survey 1-year Estimates<br>ACS-1 has wider undercertainty bands due to smaller sampling",
       x='\n% of children (under 5 y.o.) living\nunder the Federal Poverty Line in 2023\n',
       y='') + 
  scale_x_continuous(labels = percent, position='top') + 
  my_theme + 
  theme(legend.position = 'none',
        plot.title = element_textbox_simple(size=22))
```

::: {.cell-output-display}
![](index_files/figure-html/comet_state_level_plot-1.png){width=768}
:::
:::



I chose to sort states based on the percentage point change in child
poverty, year-over-year. This results in the states with the largest
**increase** in the child poverty rate on top, and states with the
largest **decrease** in the child poverty rate on the bottom. In New
Mexico, the child poverty rate increased **4.7 percentage points** from
the 2022 to the 2023 estimates from the ACS. In Vermont, the poverty
rate decreased **7 percentage points** from 2022 to 2023. Although West
Virginia saw the second largest decrease in child poverty in these
estimates, still about 1 out of every four children in the state are
below the poverty line, according to the American Community Survey.



::: {.cell}

:::



It's important to remember that all ACS estimates have a margin of error
(which by default is a 90% confidence interval), so point estimates
should be complemented with their respective error bands. ACS-1 is a
smaller sampling than the ACS-5 product, so these margins of error can
be more significant. This shows that one of *New Mexico*, *Louisiana*,
or *West Virginia* could all have the highest under-five poverty rate as
of the 2023 survey.



::: {.cell layout-align="center"}
::: {.cell-output-display}
![](index_files/figure-html/dot_plot-1.png){fig-align='center' width=1056}
:::
:::



8.  Using the `tidycensus::` margin of error aggregation functions to
    create confidence bands around the point estimate.

# A spotlight on Colorado and county-level poverty estimates



::: {.cell}

:::





Now I want to look into the movement in child poverty in Colorado
counties with the ACS-1 2023 estimates. It's important to remember that
ACS-1 estimates are limited to only geographies that exceed 65,000
people. This limits the Colorado data to only 12 counties; however,
these 12 counties constitute 88% of the children under five in the whole
state.[^1]

[^1]: This calculation is done using the 2022 ACS-5 estimates to include
    all 64 counties in Colorado.





::: {.cell}
::: {.cell-output .cell-output-stderr}

```
Warning: `stat(index)` was deprecated in ggplot2 3.4.0.
ℹ Please use `after_stat(index)` instead.
```


:::

::: {.cell-output-display}
![](index_files/figure-html/colorado_comet-1.png){width=768}
:::
:::



These changes should also be taken in the context of the margins of
error around point estimates. As shown below, the margins of error for
some counties' estimates is quite large. In **Mesa County**, the margin
of error is +/- 10% around the point estimate (of 17.8%).



::: {.cell layout-align="center"}
::: {.cell-output-display}
![](index_files/figure-html/dot_plot2-1.png){fig-align='center' width=576}
:::
:::



# Conclusion

In this `tidycensus::` post, I demonstrated:

-   How to fetch data across multiple years from the U.S. Census Bureau
    and wrangle the data for longitudinal analysis
-   How to create nice tables using the `gt::` package
-   How to construct a comet plot, thanks to some inspiration from a
    [Substack post](https://thef5.substack.com/p/how-to-comet-plot)
-   How to evaluate changes in ACS-1 estimates in the context of their
    margins of error

More to come on poverty analysis in future posts!

