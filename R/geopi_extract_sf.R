library(tidyverse)
library(sf)
library(httr2)
library(glue)
library(polite)
library(rvest)


geopi_sf2 <- function(gdot_pi){
  req_url <- request("https://rnhp.dot.ga.gov/")

  resp <- req_url%>%
    req_url_path("hosting/rest/services/GEOPI_APP/MapServer/0/query")%>%
    req_url_query(
    `f`="json",
      `where`=glue("PROJECT_ID=\'{gpi}\'",gpi=gdot_pi),
      `returnGeometry`="true",
    `spatialRel`="esriSpatialRelIntersects"
    )%>%
    req_perform()

  gpi_sf <- read_sf(resp$url)

  return(gpi_sf)
}


get_geopi_sf <-  function(gdot_pi){

  req_url <- glue("https://rnhp.dot.ga.gov/hosting/rest/services/GEOPI_APP/MapServer/0/query?f=json&where=PROJECT_ID%3D%27{gpi}%27&returnGeometry=true&spatialRel=esriSpatialRelIntersects",gpi=gdot_pi)

  gpi_sf <- read_sf(req_url)%>%
    mutate(
      Project.ID = gdot_pi
    )%>%
    group_by(Project.ID)%>%
    summarise()

  return(gpi_sf)
}




get_geopi <- function(gdot_pi, geometry = FALSE){

  session <- bow("https://www.dot.ga.gov/applications/geopi/Pages/Dashboard.aspx")

  geopi_data <- map_dfr(
    .x = gdot_pi,
    .f = function(gdot_pi=.x){

  gather_date = today()

  # table of project details (location, etc). column 1 and 3 are headings, 2 and 4 are details.
  # row 1 of all columns is the project name
  project_overview <- scrape(session, query = list(ProjectId=gdot_pi))%>%html_node("#ctl00_ctl51_g_f10b03f0_acac_4f79_94ee_72b988c6533b_ctl09 > table")%>%html_table()

  # table of project description
  project_description <- scrape(session, query = list(ProjectId=gdot_pi))%>%html_node("#ctl00_ctl51_g_fa9abf0f_0f34_4a8a_8910_0a437a68f590_ctl09 > table")%>%html_table()
  # Table of activity / program year / cost estimate / date of estimate
  phase_details <- scrape(session, query = list(ProjectId=gdot_pi))%>%html_node("#ctl00_ctl51_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46DataGrid_ctl00")%>%html_table()

  proj_desc_tidy <- bind_rows(
    tibble(name = "Project Name", value = project_overview[[1,1]]),
    project_overview[2:nrow(project_overview),c("X1","X2")]%>%rename("name"="X1","value"="X2"),
    project_overview[2:7,c("X3","X4")]%>%rename("name"="X3","value"="X4"), # nrow(project_overview)
    tibble(name = "Project Description", value = project_description[[2,1]])
  )%>%
    mutate(
      name = str_replace(name, ":", ""),
      value = na_if(value, "formatDate(\'\')"),
      value = na_if(value, "formatCurrency(\'0.0000\')"),
      value = na_if(value, "%"),
      value = na_if(value, "")
    )

  proj_desc_wide <- proj_desc_tidy%>%
    pivot_wider()%>%
    as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet=T))%>%
    mutate(
      across(
        ends_with(".Date"),
        .fns = as_date
      ),
      Gather.Date = gather_date
    )

  phase_details_wide <- phase_details%>%
    as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet=T))%>%
    mutate(
      Date.of.Last.Estimate = mdy(Date.of.Last.Estimate),
      Cost.Est.USD = str_replace_all(Cost.Estimate, "[\\$,]","")%>%as.numeric(),
      phase_abbr = str_extract(Activity,"[:alpha:]+")
    )%>%
    select(-Activity, -Cost.Estimate)%>%
    pivot_wider(
      names_from = phase_abbr,
      values_from = c(Program.Year, Date.of.Last.Estimate, Cost.Est.USD),
      names_glue = "{phase_abbr}_{.value}",
      names_vary = "slowest"
    )

  project_details_df <- bind_cols(proj_desc_wide, phase_details_wide)

  if(geometry == TRUE){

      req_url <- glue("https://rnhp.dot.ga.gov/hosting/rest/services/GEOPI_APP/MapServer/0/query?f=json&where=PROJECT_ID%3D%27{gpi}%27&returnGeometry=true&spatialRel=esriSpatialRelIntersects",gpi=gdot_pi)

      gpi_sf <- get_geopi_sf(gdot_pi)

    project_details_df <- full_join(project_details_df, gpi_sf, by = "Project.ID")%>%
      st_as_sf()

  }

  return(project_details_df)
  }

)
  return(geopi_data)

}


