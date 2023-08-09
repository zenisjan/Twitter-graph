import pandas as pd
import time
import datetime
import openexchangerates

df = pd.read_excel('Ukraine_Support_Tracker.xlsx', sheet_name = 'Clean Data', usecols = ['ID','Countries','Type of Aid General','Announcement Date','Original Currency','Monetary Value as Given by Source'])
client = openexchangerates.OpenExchangeRatesClient('')

new_df = pd.DataFrame()

for row in df.iterrows():
    
    
    id = row[1].get('ID')
    country = row[1].get('Countries')
    aid_type = row[1].get('Type of Aid General')
    date = row[1].get('Announcement Date').date()
    currency = row[1].get('Original Currency')
    value = row[1].get('Monetary Value as Given by Source')
    
    if 'USD' not in currency:
        
        exchange_rate = 1
        
        if isinstance(value, int) == False:
            value = 0
            value_usd = 0
            
        else:    
            timestamp = datetime.datetime.strptime(f"{date.day}/{date.month}/{date.year}", "%d/%m/%Y")
            exchange_rates = client.historical(timestamp,'USD')
            exchange_rate = exchange_rates.get('rates').get(currency)
            value_usd = value * (1/exchange_rate)
            
    else:
        exchange_rate = 1
        value_usd = value
        
    new_row = {'ID': id,'country': country, 'aid_type': aid_type, 'date': date, 'currency' : currency, 'value' : value, 'value_usd': value_usd, 'exchange_rate': exchange_rate}
    new_df = new_df.append(new_row, ignore_index=True)

new_df.to_excel('donations.xlsx')    