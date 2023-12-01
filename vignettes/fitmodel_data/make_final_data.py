import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import matplotlib.pyplot as plt

def main():
    df = pd.read_csv('NCAR_raw.csv')
    final_data = df[['lat', 'lon', 'time', 'rain', 'slp', 'tmp']].replace(-9999.0, 0.0).fillna(0).copy()
    final_data.columns = ['Latitude', 'Longitude', 'Time', 'Rain', 'Sea_Level_Pressure', 'Temperature']
    final_data.sort_values(by='Time', inplace=True)
    final_data.reset_index(inplace=True)
    final_data['Time'] = pd.to_datetime(final_data['Time'])
    final_data = final_data[final_data.Time.between('1999-01-01', '1999-12-31')]
    # convert timestamps to time index for inlabru
    time_index = final_data.Time.apply(lambda t: t.timestamp())
    final_data['Time_Index'] = ((time_index - time_index.min())/86400).astype(int)
    final_data['Sea_Level_Pressure'] = final_data.Sea_Level_Pressure.replace(0, np.nan).fillna(method='ffill')
    final_data.fillna(final_data['Sea_Level_Pressure'].mean(), inplace=True)
    final_data.to_csv('rain_data.csv', index=False)

if __name__=='__main__':
    main()