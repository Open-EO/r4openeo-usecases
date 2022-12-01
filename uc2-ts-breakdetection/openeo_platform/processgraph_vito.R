# process graph for getting s2 ndvi from VITO Dev backend and
# running bfast.

# extent: either test.json (100x100 px) or vaja.json
# time: 2016 to 2021

# libs -------------------------------------------------------------------------
library(openeo)
library(dplyr)
library(sf)

# connect to backend -----------------------------------------------------------
host = "https://openeo-dev.vito.be" # "https://openeo.cloud" # "https://openeo-dev.vito.be"
con = openeo::connect(host)
login()

# con$isConnected()
# con$isLoggedIn()
# describe_account()

# definitions ------------------------------------------------------------------

# output file
name_out = "test.nc"
file_format = "netCDF"
pth_out = "~/git_projects/r4openeo-usecases/uc2-ts-breakdetection/openeo_platform/results"
pth_out = file.path(pth_out, name_out)

# path to spatial extent
pth_ext = "~/git_projects/r4openeo-usecases/uc2-ts-breakdetection/00_extent/test.json"

# pth_udf
pth_udf = "https://raw.githubusercontent.com/Open-EO/r4openeo-usecases/main/uc2-ts-breakdetection/00_udfs/bfast_udf_handle_null_vito.R"

# define parameters ------------------------------------------------------------
collection = "SENTINEL2_L2A_SENTINELHUB"
spatial_extent = jsonlite::read_json(pth_ext)
time_range = list("2016-01-01", 
                  "2021-01-01")
bands = c("B04", "B08", "SCL")
bands_cloud = "SCL"

# define process graph ---------------------------------------------------------
p = openeo::processes()

data = p$load_collection(id = collection, 
                         spatial_extent = list(west = spatial_extent$spatial_extent$west, 
                                               east = spatial_extent$spatial_extent$east, 
                                               south = spatial_extent$spatial_extent$south, 
                                               north = spatial_extent$spatial_extent$north),
                         temporal_extent = time_range, 
                         bands = bands) 

data_clouds = p$filter_bands(data = data, bands = bands_cloud)

fun_cloud_mask = function(data, context) {
  scl = data
  res = scl == 3 | scl == 8 | scl == 9
  return(res)
}

cloud_mask = p$apply(data = data_clouds, process = fun_cloud_mask)

mask_clouds = p$mask(data = data, mask = cloud_mask)

calc_ndvi = p$reduce_dimension(data = mask_clouds,
                               dimension = "bands", 
                               reducer = function(data, context) {
                                 red = data[1]
                                 nir = data[2]
                                 (nir-red)/(nir+red)})

reduce_udf = p$reduce_dimension(data = calc_ndvi, 
                                dimension = "t", # at Eurac it's called DATE, at VITO "time" or "t" 
                                reducer = function(data, context){
                                  p$run_udf(data = data, 
                                            context = list(start_monitor = 2018),
                                            udf = pth_udf, 
                                            runtime = "python")}) # at VITO has to be set to python and some boilerplate code added to the R-UDF

ma <- function(data, context){
  p$run_udf(data = data, udf = readr::read_file("src/udf.py"),
            runtime = "Python"



# reduce_udf_magn = p$reduce_dimension(data = calc_ndvi, 
#                                      dimension = "t",
#                                      reducer = function(data, context){
#                                        p$run_udf(data = data, 
#                                                  context = list(start_monitor = 2018, 
#                                                                 val = "magnitude"),
#                                                  udf = pth_udf, 
#                                                  runtime = "R")})
# 
# fun_magn_mask = function(data, context) { 
#   magn = data 
#   res = magn > -0.2 
#   return(res)
# }
# 
# magn_mask = p$apply(data = reduce_udf_magn, process = fun_magn_mask)
# 
# mask_magn = p$mask(data = reduce_udf, mask = magn_mask)

result = p$save_result(data = mask_magn, format=file_format) # "GTiff" #"netCDF"


result = p$save_result(data = calc_ndvi, format="netCDF") # "GTiff"
#pg_json = as(result, "Process")


# create job -------------------------------------------------------------------
job = create_job(graph = result,
                 title = name_out,
                 description = name_out,
                 format = file_format)
start_job(job = job$id)

# status job -------------------------------------------------------------------
status(job)
result_obj = list_results(job = job$id)
dwnld = download_results(job = job$id, folder = pth_out)

# quick check ------------------------------------------------------------------
library("stars")
file.size(dwnld[[1]])/1000000 # mb
tst = read_stars(dwnld[[1]])
tst
plot(tst)

# vaja on vito-dev for 2016 to 2021 (5GB)
# CPU usage
# 933,550 cpu-seconds
# Wall time
# 4,775 seconds
# Memory usage
# 2,377,836,590 mb-seconds
# Sentinelhub
# 38,620.25 sentinelhub_processing_unit

# test on vito-dev for 2016 to 2021 (10MB)
# CPU usage
# 26,126 cpu-seconds
# Wall time
# 216 seconds
# Memory usage
# 66,473,528 mb-seconds
# Sentinelhub
# 386 sentinelhub_processing_unit

# end --------------------------------------------------------------------------

# note on cloud masking:
# actually the CLM band is the cloud mask with 0 = no cloud, 1 = cloud, 255 = no data
# and should be possible to feed it directly into mask. There are errors though on VITO
# (https://discuss.eodc.eu/t/cloud-masking-sentinel2-l2a-sentinelhub-with-mask-and-clm-band/488)
# cloud mask description on sentinel hub: https://docs.sentinel-hub.com/api/latest/user-guides/cloud-masks/

# more notes on cloud masking:
# It would be interesting to save the different cloud masks
# test_clp2 -> Cloud masking CLP band /255 > 0.3
# test_clp3 -> Clod masking CLP band /255 > 0.1
# test_cld -> Cloud masking CLD band > 0.3
# test_clm -> Save only the CLM Band with adapted temporal resolution
# true color composit!
# -> save the masks and true color composit and compare which one is best suitable

# process node to load cloud mask seperately:
# bands = c("B04", "B08")
# bands_cloud = "CLM" # 0 = no cloud, 1 = cloud, 255 = no data -> the mask process should handle this
# data_clouds = p$load_collection(id = collection, 
#                                 spatial_extent = list(west = spatial_extent$spatial_extent$west, 
#                                                       east = spatial_extent$spatial_extent$east, 
#                                                       south = spatial_extent$spatial_extent$south, 
#                                                       north = spatial_extent$spatial_extent$north),
#                                 temporal_extent = time_range, 
#                                 bands = bands_cloud) 


# bands for clp
# bands = c("B04", "B08", "CLP")
# bands_cloud = "CLP"

# time range used for testing
# time_range = list("2018-04-01",
#                   "2018-05-01")

# fun to mask using clp
# fun_cloud_mask = function(data, context){
#   (data/255) > 0.3
# }
# 
