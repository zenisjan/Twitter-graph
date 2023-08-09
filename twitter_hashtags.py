import tweepy
import configparser
import csv
import logging
from datetime import date, timedelta, datetime

config = configparser.ConfigParser()
config.read('config_hashtag.ini')

start_date = datetime.strptime(config['twitter']['start_date'],'%Y-%m-%d').date()
end_date = datetime.strptime(config['twitter']['end_date'],'%Y-%m-%d').date()
barrer_token_config = config['twitter']['barrer_token']

# all_queries = ['russia', 'ukraine', 'donetsk', 'putin', 'zelensky', 'lviv', 'ukrainewar', 'kherson', 'kharkiv', 'luhansk', 'crimea', 'zaporizhia', 'wagner', 'ukrainerussiawar', 'bakhmut', 'svatove', 'kreminna', 'kremlin', 'prigozhin', 'gerasimov', 'bakhmut', 'soledar']
# all_queries = ["Constanta","Sinop","Istanbul","Kirklareli","Kocaeli","Donetsk","Luhansk","Kirovograd","Zhytomyr","Poltava","Cherkasy","Dnipropetrovsk","Rivne","Sumy","Ternopil","Zaporizhzhia","Kharkiv","Kherson","Lviv","Odesa","Crimea","Chernihiv","Vinnytsia","Chernivtsi","Volyn","Kyiv","Khmelnytskyi","Zakarpattia","Ivano-Frankivsk","Mykolaiv"]
# all_queries = ['ukraine','russia']
all_queries = ['zaporizhia','zaporizhzhia']


logging.basicConfig(filename='TwitterScraperLog',
                    filemode='a',
                    format='%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                    datefmt='%H:%M:%S',
                    level=logging.INFO)



client = tweepy.Client(bearer_token=barrer_token_config, wait_on_rate_limit=True)
      
# Process all tweets from start date to end date parameter in four hour long intervals based on the search query
def process_datarange():
    
    with open('tweet_count_zaporizhzhia.csv', 'a+') as file:
        
        header = ['date', 'count', 'query']
            
        # create the csv writer
        writer = csv.writer(file)
        
        writer.writerow(header)
        
        global start_date
        original_start_date = start_date
        
        for query in all_queries:
            
            while start_date < end_date:
                
                start_timedate = '{}-{}-{}T00:00:01Z'.format(start_date.year,start_date.month,start_date.day)
                
                if int((end_date - start_date).days)  < 31:
                    x = int((end_date - start_date).days)
                    start_date = start_date + timedelta(days= x)
                else:    
                    start_date = start_date + timedelta(days=31)
                    
                end_timedate = '{}-{}-{}T23:59:59Z'.format(start_date.year,start_date.month,start_date.day)
                
                counts = client.get_all_tweets_count(query=query, start_time = start_timedate, end_time = end_timedate, granularity='day').data

                for count in counts:
                    row = [count.get('start')[0:10], count.get('tweet_count'), query]
                    # write a row to the csv file
                    writer.writerow(row)
                    
            start_date = original_start_date
                    
        
    
            
def main():
    process_datarange()
  
if __name__== "__main__":
  main()
    

