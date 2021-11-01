#' xanthos_to_gcam_xml
#'
#' This function creates xmls for GCAM from raw xanthos outputs
#' a given dataframe. Returns a long dataframe with lat lon and parameter from the netcdf chosen.
#' @param xanthos_runoff_csv Default = NULL. Path to xanthos basin runoff .csv outputs file.
#' @param gcamdata_folder Default = NULL. Path to gcamdatafolder.
#' @param out_dir Default = NULL. Path to folder to save outputs in.
#' @importFrom magrittr %>%
#' @source gcamdata zchunk_L201.water_resources_constrained
#' @return xml
#' @export

xanthos_to_gcam_xml <- function(xanthos_runoff_csv = NULL,
                                    gcamdata_folder = NULL,
                                    out_dir = NULL) {


  print("Starting xanthos_to_gcam_xml ...")

  # Initialize
  NULL -> Basin_name -> GCAM_basin_ID -> GCAM_region_ID -> GLU -> ISO ->
    basin_id -> basin_name -> bind_rows -> id -> iso -> maxSubResource ->
    name -> region -> renewresource -> sub.renewable.resource -> water_type ->
    year


  # Read in xanthos output file and region files
  gcamdatafolder = gcamdata_folder
  dfraw <- data.table::fread(xanthos_runoff_csv,header=TRUE)

  # Prepare the Data by GCAM Basin Region
  if(T){
    # From .input/gcamdata/R/zchunk_L201.water_resources_constrained
    GCAM_region_names <- data.table::fread(paste(gcamdatafolder,"/inst/extdata/common/GCAM_region_names.csv",sep=""),header=TRUE)
    iso_GCAM_regID <- data.table::fread(paste(gcamdatafolder,"/inst/extdata/common/iso_GCAM_regID.csv",sep=""),header=TRUE)
    basin_to_country_mapping <- data.table::fread(paste(gcamdatafolder,"/inst/extdata/water/basin_to_country_mapping.csv",sep=""),header=TRUE)
    basin_ids <- data.table::fread(paste(gcamdatafolder,"/inst/extdata/water/basin_ID.csv",sep=""),header=TRUE)
    water_mapping_R_GLU_B_W_Ws_share <- data.table::fread(paste(gcamdatafolder,"/outputs/L103.water_mapping_R_GLU_B_W_Ws_share.csv",sep=""),header=TRUE)
    water_mapping_R_B_W_Ws_share <- data.table::fread(paste(gcamdatafolder,"/outputs/L103.water_mapping_R_B_W_Ws_share.csv",sep=""),header=TRUE)

    # Basin_to_country_mapping table include only one set of dplyr::distinct basins
    # that are mapped to a single country with largest basin share.
    # Assign GCAM region name to each basin.
    # Basin with overlapping GCAM regions assign to region with largest basin area.
    basin_to_country_mapping %>%
      dplyr::rename(iso = ISO) %>%
      dplyr::mutate(iso = tolower(iso)) %>%
      dplyr::left_join(iso_GCAM_regID, by = "iso") %>%
      # ^^ non-restrictive join required (NA values generated for unmapped iso)
      # basins without gcam region mapping excluded (right join)
      # Antarctica not assigned
      dplyr::right_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      dplyr::rename(basin_id = GCAM_basin_ID,
                    basin_name = Basin_name) %>%
      dplyr::select(GCAM_region_ID, region, basin_id) %>%
      dplyr::arrange(region) ->
      RegionBasinHome

    # identify basins without gcam region mapping (dplyr::anti_join)
    basin_to_country_mapping %>%
      dplyr::rename(iso = ISO) %>%
      dplyr::mutate(iso = tolower(iso)) %>%
      dplyr::left_join(iso_GCAM_regID, by = "iso") %>%
      #not all iso included in basin mapping
      # ^^ non-restrictive join required (NA values generated for unmapped iso)
      dplyr::anti_join(GCAM_region_names, by = "GCAM_region_ID") ->
      BasinNoRegion

    # create full set of region/basin combinations
    # some basins overlap multiple regions
    # Use left join to ensure only those basins in use by GCAM regions are included
    bind_rows(water_mapping_R_GLU_B_W_Ws_share %>%
                dplyr::rename(basin_id = GLU),
              water_mapping_R_B_W_Ws_share) %>%
      dplyr::select(GCAM_region_ID, basin_id, water_type) %>%
      dplyr::filter(water_type == "water withdrawals") %>%
      dplyr::distinct() %>%
      dplyr::left_join(basin_ids, by = "basin_id") %>%
      tibble::as_tibble()%>%
      # ^^ non-restrictive join required (NA values generated for unused basins)
      gcamdata::left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      dplyr::mutate(water_type = "water withdrawals",
                    resource = paste(basin_name, water_type, sep="_")) %>%
      dplyr::arrange(region, basin_name) ->
      L201.region_basin

    # create unique set of region/basin combination with
    # basin contained by home region (region with largest basin area)
    L201.region_basin %>%
      dplyr::inner_join(RegionBasinHome, by = c("basin_id","GCAM_region_ID","region")) %>%
      dplyr::arrange(region, basin_name) ->
      L201.region_basin_home

    # Re-format to format for ./input/gcamdata/outputs/L201.GrdRenwRsrcMax_runoff.csv which include
    # region, renewresource, sub.renewable.resource, year, maxSubResource

    df <- L201.region_basin_home %>%
      dplyr::left_join(dfraw %>%
                         tidyr::gather(key="year",value="maxSubResource",-id,-name) %>%
                         dplyr::filter(year %in% c(1975,1990,seq(2005,2100,by=5))) %>%
                         dplyr::rename(basin_id=id),by="basin_id") %>%
      dplyr::mutate(sub.renewable.resource="runoff")%>%
      dplyr::select(id=basin_id,region=region,renewresource=basin_name, sub.renewable.resource, year,maxSubResource);

    df %>% dplyr::filter(is.na(renewresource), year==1975)
    df %>% dplyr::filter(year==1975) %>% nrow()
    df %>% dplyr::filter(region=="USA",year==1975)%>%dplyr::arrange(renewresource)
    df %>% dplyr::filter(region=="China",year==1975)%>%dplyr::arrange(renewresource)
    df %>% dplyr::filter(region=="Southeast Asia",year==1975)%>%dplyr::arrange(renewresource)
    df %>% dplyr::filter(renewresource=="Hong-Red River")%>%dplyr::arrange(renewresource)
    df$year%>%unique()
    df <- df %>%
      dplyr::mutate(renewresource = paste0(renewresource,"_water withdrawals"))
  }

  # Save as xml
  if(T){
    # Use header: GrdRenewRsrcMaxNoFillOut from .\input\gcamdata\inst\extdata\mi_headers\ModelInterface_headers

    if(is.null(out_dir)){
      out_dirx <- dirname(xanthos_runoff_csv)
    } else if (dir.exists(out_dir)){
      out_dirx  <- out_dir
    } else {
      print(paste0("out_dir provided does not exist: ", out_dir))
      print(paste0("Saving in : ", dirname(xanthos_runoff_csv)))
      out_dirx <- dirname(xanthos_runoff_csv)
    }


    fname <- paste0(out_dirx,"/",gsub(".csv",".xml",basename(xanthos_runoff_csv))); fname

    gcamdata::create_xml(fname) %>%
      gcamdata::add_xml_data(df, "GrdRenewRsrcMaxNoFillOut")%>%
      gcamdata::run_xml_conversion()

    print(paste0("File saved as ",fname))
  }


  print("Finished xanthos_gcam_create_xml.")

  invisible(df)

}
