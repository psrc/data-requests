# This script determines if a parcel is within the Metro Frequent Route Buffer
# Created by Puget Sound Regional Council Staff
# February 2022

import h5py
import pandas as pd 
import os
import geopandas as gp
from simpledbf import Dbf5

working_directory = os.getcwd()

years = ['2018','2030','2040', '2050']

state_plane = 'epsg:2926'

# File
route_buffers = os.path.join(working_directory,'data','buffers','frequent-buffers.shp')
route_dbf = os.path.join(working_directory,'data','buffers','frequent-buffers.dbf')

parcels_file = os.path.join(working_directory,'data','parcels','parcel-points.shp')

def create_df_from_h5(h5_file, h5_table, h5_variables):

    h5_data = {}
    
    for var in h5_variables:
        h5_data[var] = h5_file[h5_table][var][:]
    
    return pd.DataFrame(h5_data)

print('Creating a Route Buffer dataframe')
buffers = Dbf5(route_dbf)
buffers = buffers.to_dataframe()
buffers = buffers['RTNAME']

print('Opening Parcels Points File')
final_cols = ['parcel_id', 'fips', 'geometry']
parcels_layer = gp.GeoDataFrame.from_file(parcels_file)
parcels_layer = parcels_layer[final_cols]
parcels_layer.rename(columns={'parcel_id': 'parcelid', 'fips': 'county'}, inplace=True)

current_count = 0
for scenario in years:
    print ('Creating parcel file with people and jobs for '+scenario)
    
    person_variables=['hhno']
    hh_variables=['hhno','hhparcel']
    parcel_cols = ['parcelid','emptot_p']
    
    # Create Scenario Specific Filenames
    parcel_file = working_directory+'\\data\\'+scenario+'\\parcels_urbansim.txt'
    hh_person = working_directory+'\\data\\'+scenario+'\\hh_and_persons.h5'
    
    # Create a parcel dataframe
    wrk_prcls = pd.read_csv(parcel_file, sep = ' ')
    wrk_prcls.columns = wrk_prcls.columns.str.lower()
    wrk_prcls = wrk_prcls.loc[:,parcel_cols]

    # Create HH and Person dataframes from the h5 File
    hh_people = h5py.File(hh_person,'r+') 
    hh_df = create_df_from_h5(hh_people, 'Household', hh_variables)
    person_df = create_df_from_h5(hh_people, 'Person', person_variables)

    # Create a HH file by household number with total population
    person_df['population'] = 1
    df_hh = person_df.groupby('hhno').sum()
    df_hh = df_hh.reset_index()

    # Merge the HH File created from the persons with the original HH file
    df_hh = pd.merge(df_hh,hh_df,on='hhno',suffixes=('_x','_y'),how='left')
    df_hh.rename(columns={'hhparcel': 'parcelid'}, inplace=True)
    fields_to_remove=['hhno']
    df_hh = df_hh.drop(fields_to_remove,axis=1)

    # Group the HH Files by Parcel ID so it can be merged with master parcel file
    df_parcel_hh = df_hh.groupby('parcelid').sum()
    df_parcel_hh = df_parcel_hh.reset_index()

    # Merge the Full Parcel File with X,Y with the parcel file from the HH's
    wrk_prcls = pd.merge(wrk_prcls,df_parcel_hh,on='parcelid',suffixes=('_x','_y'),how='left')
    wrk_prcls.fillna(0,inplace=True)
    wrk_prcls.rename(columns={'emptot_p': 'employment'}, inplace=True)
    updated_columns = ['parcelid','jobs_'+scenario, 'population_'+scenario]
    wrk_prcls.columns = updated_columns

    if current_count == 0:
        initial_parcels = wrk_prcls
            
    else:
        initial_parcels = pd.merge(initial_parcels, wrk_prcls, on='parcelid',suffixes=('_x','_y'),how='left')
    
    current_count = current_count + 1

# Merge the parcels with X,Y datafame and create a column for route buffer flag
parcels_layer = pd.merge(parcels_layer, initial_parcels, on='parcelid',suffixes=('_x','_y'),how='left')
parcels_layer.fillna(0,inplace=True)

# Now Figure out how many people and jobs are in each route buffer by spatial joining parcels and route buffers
current_count = 0
for working_route in buffers:
    print('Working on buffers for Route ' + working_route)
    
    # Clip Buffer to be a specific route
    df = gp.GeoDataFrame.from_file(route_buffers)
    route_buffer = df[df['RTNAME']==working_route]
   
    # Spatial Join the Current Buffer with the Parcel Layer
    final_cols = ['RTNAME','population_2018','population_2030','population_2040','population_2050','jobs_2018','jobs_2030','jobs_2040','jobs_2050']
    merged = gp.sjoin(route_buffer, parcels_layer, how = "inner", op='intersects')
    merged = merged.loc[:,final_cols]
    
    # Group by RTNAME for a Total Summary for the Route Buffer
    consolidated_df = merged.groupby('RTNAME').sum()
    consolidated_df = consolidated_df.reset_index()
    consolidated_df.fillna(0,inplace=True)

    if current_count == 0:
        final_df = consolidated_df
            
    else:
        final_df = final_df.append(consolidated_df)
    
    current_count = current_count + 1

print('Figure out King County Shares by Year')
king_parcels = parcels_layer[parcels_layer['county']==33]
pop_2018 = sum(king_parcels['population_2018'])
pop_2030 = sum(king_parcels['population_2030'])
pop_2040 = sum(king_parcels['population_2040'])
pop_2050 = sum(king_parcels['population_2050'])

job_2018 = sum(king_parcels['jobs_2018'])
job_2030 = sum(king_parcels['jobs_2030'])
job_2040 = sum(king_parcels['jobs_2040'])
job_2050 = sum(king_parcels['jobs_2050'])

print('Adding Share of County Total in each buffer for each Year')
final_df['Population_Share_2018'] = final_df['population_2018']/pop_2018
final_df['Population_Share_2030'] = final_df['population_2030']/pop_2030
final_df['Population_Share_2040'] = final_df['population_2040']/pop_2040
final_df['Population_Share_2050'] = final_df['population_2050']/pop_2050

final_df['Job_Share_2018'] = final_df['jobs_2018']/job_2018
final_df['Job_Share_2030'] = final_df['jobs_2030']/job_2030
final_df['Job_Share_2040'] = final_df['jobs_2040']/job_2040
final_df['Job_Share_2050'] = final_df['jobs_2050']/job_2050

print('Output final file')
final_df.to_csv(working_directory + '\\pop_jobs_frequent_transit.csv',index=False)
