PAGE_SIZE <- 2000

get_earthdata_token <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(NULL)
  tryCatch({
    earthaccess <- reticulate::import("earthaccess")
    auth <- earthaccess$login()
    auth$token[["access_token"]]
  }, error = function(e) NULL)
}

ATLAS_dataFinder_direct <- function(short_name,
                             lower_left_lon,
                             lower_left_lat,
                             upper_right_lon,
                             upper_right_lat,
                             version = "006",
                             daterange = NULL,
                             cloud_hosted = TRUE) {
  `%>%` <- magrittr::`%>%`
  token <- get_earthdata_token()
  auth_header <- if (!is.null(token)) c(Authorization = paste("Bearer", token)) else c()
  bbox <- paste(lower_left_lon, lower_left_lat, upper_right_lon, upper_right_lat, sep = ",")
  collection_search_url <- if (cloud_hosted) {
    sprintf(
      "https://cmr.earthdata.nasa.gov/search/collections.json?short_name=%s&version=%s&cloud_hosted=true",
      short_name, version
    )
  } else {
    sprintf(
      "https://cmr.earthdata.nasa.gov/search/collections.json?short_name=%s&version=%s",
      short_name, version
    )
  }
  handle <- curl::new_handle(httpheader = auth_header)
  response <- curl::curl_fetch_memory(collection_search_url, handle = handle)
  collections_json <- response$content %>%
    rawToChar() %>%
    jsonlite::parse_json()
  collections_ids <- sapply(collections_json$feed$entry, function(x) x$id)
  url_format <- paste0(
    "https://cmr.earthdata.nasa.gov/search/granules.json?",
    "pretty=false&page_size=%s&short_name=%s",
    "&bounding_box=%s&version=%s%%s"
  )
  request_url <- sprintf(url_format, PAGE_SIZE, short_name, bbox, version)
  temporal_filter <- ""
  if (!is.null(daterange)) {
    temporal_filter <- sprintf("&temporal=%s,%s", daterange[1], daterange[2])
  }
  request_url <- request_url %>% sprintf(temporal_filter)
  granules_href <- sapply(collections_ids, function(x) fetchAllGranules(request_url, x, auth_header))
  return(granules_href)
}

fetchAllGranules <- function(request_url, collection_id, auth_header = c()) {
  granules_href <- c()
  page <- 1
  totalHits <- 0
  repeat {
    handle <- curl::new_handle(httpheader = auth_header)
    response <- curl::curl_fetch_memory(paste0(
      request_url,
      "&pageNum=",
      page,
      "&collection_concept_id=",
      collection_id
    ), handle = handle)
    if (totalHits == 0) {
      totalHits <- as.integer(curl::parse_headers_list(response$headers)[["cmr-hits"]])
    }
    result <- rawToChar(response$content) %>%
      jsonlite::parse_json()
    if (response$status_code != 200) {
      stop(paste("\n", result$errors, collapse = "\n"))
    }
    granules <- result$feed$entry
    if (length(granules) == 0) break
    hrefs <- sapply(granules, function(x) x$links[[1]]$href)
    granules_href <- c(granules_href, hrefs)
    if ((page * PAGE_SIZE) >= totalHits) break
    page <- page + 1
  }
  return(granules_href)
}
