library(getSpatialData)

get_ls <- function (time_range, products, aoi = NULL, as_sf = TRUE, rename_cols = TRUE, 
          check_products = TRUE, simplify_cols = TRUE, ..., verbose = TRUE) 
{
  if (missing(products)) {
    extras <- list(...)
    if (!is.null(extras$check_avail)) 
      getSpatialData:::out("Argument 'check_avail' is deprecated. Use check_availability() to check whether records are available on-demand or not.", 
          type = 2)
    products <- extras$name
    if (is.null(products)) 
      products <- extras$platform
  }
  products <- tolower(products)
  getSpatialData:::.check_verbose(verbose)
  getSpatialData:::.check_time_range(time_range)
  if (isTRUE(check_products)) 
    getSpatialData:::.check_products(products, products_available = get_products(update_online = F))
  clients <- sapply(products, function(x) {
    if (grepl("sentinel", x)) 
      return("CopHub")
    if (grepl("landsat", x)) 
      return("EE")
    if (grepl("modis", x)) 
      return("EE")
    if (grepl("srtm", x)) 
      return("CMR")
  })
  if (is.null(unlist(clients))) 
    getSpatialData:::out("Could not find appropriate client(s) for this/these product(s). This/these product(s) is/are not supported.", 
        type = 3)
  records <- mapply(client = clients, product_name = products, 
                    function(client, product_name) {
                      if (is.null(client)) 
                        getSpatialData:::out(paste0("Could not find appropriate client for product '", 
                                   product_name, "'. This product is not supported."), 
                            type = 2)
                      else {
                        eval(parse(text = paste0("getSpatialData:::.records_", client, 
                                                 "(time_range = time_range, product_name = product_name, aoi = aoi, rename_cols = rename_cols, simplify_cols = simplify_cols, ..., verbose = verbose)")))
                      }
                    }, USE.NAMES = F, SIMPLIFY = F)
  records <- records[!sapply(records, is.null)]
  if (length(records) == 0) {
    records <- NULL
  }
  else {
    if (length(records) > 1) 
      records <- rbind.different(.gsd_compact(records))
    else records <- records[[1]]
  }
  if (!is.null(records)) {
    getSpatialData:::out(paste0("Found a total of ", nrow(records), " records."))
  # records <- getSpatialData:::.make_tileid_landsat(records)
    if (all(is.na(records$tile_id))) 
      records$tile_id <- NULL
    if (is.null(records$level)) {
      records$level <- NA
    }
    used_names <- sapply(unique(getOption("gSD.clients_dict")$gSD),
                         function(x) x %in% colnames(records))
    sorted_names <- unique(getOption("gSD.clients_dict")$gSD)[used_names]
    undefined_names <- colnames(records)[!sapply(colnames(records),
                                                 function(x) x %in% sorted_names, USE.NAMES = F)]
    if (length(undefined_names) > 0) {
      records <- cbind(records[, sorted_names], records[,
                                                        undefined_names])
      colnames(records) <- c(sorted_names, undefined_names)
    }
    if (is.null(records$footprint)) {
      out("Retrieved records have no spatial footprint and thus argument 'as_sf' is ignored.",
          type = 2)
      as_sf <- FALSE
    }
    return(getSpatialData:::.check_records(records, as_sf = as_sf))
  }
}
