"""
The Zip codes were found using the package pgeocode as we had to determine the latitude and longitude of a 3 digit zip.
"""

import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pgeocode
from shapely.geometry import Point, MultiPolygon, Polygon
import shapely
import random


## The input file is prepared from the underlying raw demog data that has participant self-reported zipcode data
df = pd.read_csv(r"brighten_participant_zipcodes.tsv", sep='\t')



lat_long={}
usa_codes = pgeocode.Nominatim('us')
zipcode = usa_codes._data_frame
zipcode['zip'] = zipcode['postal_code'].str[:3]
def get_latlong(zip):
    if zip in lat_long:
        return lat_long[zip]
    else:
        lat_long[zip] = (np.median(zipcode[zipcode['zip'] == zip]['latitude'].to_numpy()),np.median(zipcode[zipcode['zip'] == zip]['longitude'].to_numpy()))
zipcode['median_zip'] = zipcode['zip'].apply(lambda x: get_latlong(x))
df = df[df['zipcode'].fillna(0).astype(int).astype(str).isin(list(lat_long.keys()))]

df['median_zip'] = df['zipcode'].apply(lambda x: lat_long[str(int(x)).zfill(3)])
df= df[df['median_zip'].notna()]




df['median_lat'] = df['median_zip'].apply(lambda x: x[0] +random.random()/2-.25)
df['median_long'] = df['median_zip'].apply(lambda x: x[1]+random.random()/2-.25)
df['median_long'] = [med if state != 'HI' else med+52 for med, state in zip(df['median_long'],df['state'])]
df['median_lat'] = [med if state != 'HI' else med+5 for med, state in zip(df['median_lat'],df['state'])]

df['median_long'] = [med if state != 'AK' else ((med+35)+117)*.35 -117 for med, state in zip(df['median_long'],df['state'])]
df['median_lat'] = [med if state != 'AK' else ((med-36)-27)*.35 + 27 for med, state in zip(df['median_lat'],df['state'])]



usa = gpd.read_file(r'C:\Users\calvi\Downloads\beer-locations\data-analysis\visualization\shapefiles\states_21basic\states.shp')

fig, ax = plt.subplots(figsize=(30,30))
usa.at[0,'geometry'] =  MultiPolygon((shapely.affinity.translate(x,52,5) for x in usa.loc[0]['geometry']))
usa.at[50,'geometry'] =  MultiPolygon((shapely.affinity.scale(shapely.affinity.translate(x,33,-36),.35,.35) for x in usa.loc[50]['geometry']))

usa.plot(ax = ax, alpha = 0.3, color = '#d6d5d4', edgecolor = '#525252')
for val in ['screened', 'enrolled']:
    plt.scatter(df[df['status']==val]['median_long'].to_numpy(),df[df['status']==val]['median_lat'].to_numpy(), s = 1, c='orange' if val == 'enrolled' else 'blue', label = val)

lgnd = ax.legend(loc='lower right', prop={'size': 16})
lgnd.legendHandles[0]._sizes = [30]
lgnd.legendHandles[1]._sizes = [30]
plt.xlim((-130,-60))
plt.ylim(22,50)
plt.axis('off')

plt.show()