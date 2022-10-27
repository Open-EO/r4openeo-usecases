import xarray as xr
import numpy as np
import requests
import dask
from dask import delayed as dask_delayed
from threaded import call_r

def print_func(data, dimensions, labels, file, process, dimension, context):
    print(data)
    print('+++++++++')
    return
    
def execute_udf(process, udf, data, dimension = None, context = None, parallelize = False, chunk_size = 1000):
    # Prepare UDF code
    udf_filename = prepare_udf(udf)

    # Prepare data cube metadata
    input_dims = list(data.dims)
    output_dims = list(data.dims)
    if dimension is not None:
        output_dims.remove(dimension)
    kwargs_default = {'process': process, 'dimension': dimension, 'context': context, 'file': udf_filename, 'dimensions': list(),  'labels': list()}

    if process == 'apply' or process == 'reduce_dimension':
        def runnable(data): 
            kwargs = kwargs_default.copy()
            kwargs['dimensions'] = list(data.dims)
            kwargs['labels'] = get_labels(data)
            #return call_r(data.values, kwargs['dimensions'], kwargs['labels'], udf_filename, process, dimension, context)
            return xr.apply_ufunc(
                call_r, data, kwargs = kwargs,
                input_core_dims = [input_dims], output_core_dims = [output_dims],
                vectorize = False
               #exclude_dims could be useful for apply_dimension?
            )

        if parallelize:
            # Chunk data
            chunks = chunk_cube(data, dimension = dimension, size = chunk_size)
            # Execute in parallel
            dask_calls_list = []
            for chunk in chunks:
                dask_calls_list.append(dask_delayed(runnable)(chunk))
            chunks = dask.compute(*dask_calls_list)
            # Combine data again
            return combine_cubes(chunks)
        else:
            # Don't parallelize
            return runnable(data)
    else:
        raise Exception("Not implemented yet for Python")

def get_labels(data):
    labels = []
    for k in data.dims:
        labels.append(data.coords[k].data)
    return labels

def create_dummy_cube(dims, sizes, labels):
    npData = np.random.rand(*sizes)
    if (labels['x'] is None):
        labels['x'] = np.arange(npData.shape[0])
    if (labels['y'] is None):
        labels['y'] = np.arange(npData.shape[1])
    xrData = xr.DataArray(npData, dims = dims, coords = labels)
    return xrData

def save_result(data, file):
    return data.to_netcdf(file)

def combine_cubes(data):
    return xr.combine_by_coords(
        data_objects = data,
        compat = 'no_conflicts',
        data_vars = 'all',
        coords = 'different',
        join = 'outer',
        combine_attrs = 'no_conflicts',
        datasets = None)

def chunk_cube(data, dimension = None, size = 1000):
    # todo: generalize to work on all dimensions except the one given in `dimension`
    chunks = []
    data_size = dict(data.sizes)
    num_chunks_x = int(np.ceil(data_size['x']/size))
    num_chunks_y = int(np.ceil(data_size['y']/size))
    for i in range(num_chunks_x):
        x1 = i * size
        x2 = min(x1 + size, data_size['x']) - 1
        for j in range(num_chunks_y):
            y1 = j * size
            y2 = min(y1 + size, data_size['y']) - 1
            chunk = data.loc[dict(
                x = slice(data.x[x1], data.x[x2]),
                y = slice(data.y[y1], data.y[y2])
            )]
            chunks.append(chunk)


    return chunks

def generate_filename():
    return "./udfs/temp.R" # todo

def prepare_udf(udf):
    if isinstance(udf, str) == False :
        raise "Invalid UDF specified"

    if udf.startswith("http://") or udf.startswith("https://"): # uri
        r = requests.get(udf)
        if r.status_code != 200:
            raise Exception("Provided URL for UDF can't be accessed")
        
        return write_udf(r.content)
    elif "\n" in udf or "\r" in udf: # code
        return write_udf(udf)
    else: # file path
        return udf

def write_udf(data):
    filename = generate_filename()
    success = False
    file = open(filename, 'w')
    try:
        file.write(data)
        success = True
    finally:
        file.close()
    
    if success == True:
        return filename
    else:
        raise Exception("Can't write UDF file")
