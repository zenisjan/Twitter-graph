import tweepy
import configparser
import json
import logging
import psycopg2
from datetime import date, timedelta, datetime

config = configparser.ConfigParser()
config.read('config.ini')

max_tweets_per_day = int(config['twitter']['max_tweets_per_day'])
start_date = datetime.strptime(config['twitter']['start_date'],'%Y-%m-%d').date()
end_date = datetime.strptime(config['twitter']['end_date'],'%Y-%m-%d').date()
barrer_token_config = config['twitter']['barrer_token']
query = config['twitter']['search_query']



logging.basicConfig(filename='TwitterScraperLog',
                    filemode='a',
                    format='%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                    datefmt='%H:%M:%S',
                    level=logging.INFO)



client = tweepy.Client(bearer_token=barrer_token_config, wait_on_rate_limit=True)

#This function create a generator that enables to loop through every day in the range specified by parameters 'start date' and 'end date'
def daterange(start, end):
    for n in range(int((end - start).days)):
        yield start + timedelta(n)

#get all the data from the list of dicts by the key
def getDataList (list, set, key):
    entities = ''
    
    if list is None:
        return ''
    
    tweet_parameter = list.get(set)
    if tweet_parameter:
        for item in tweet_parameter:
            entities += item.get(key)
            entities += ' '
    return entities


#This funciton returns all the Tweet author data based on the Api call to Twitter v2 or based on the already stored data in DB 
def getTweetAuthorData (tweet_author_id):
    
    logging.info('Tweet Author ID %s', tweet_author_id)

    #Check if the Author already exist in DB
    row = check_existence_by_ID('author', tweet_author_id)
    if row:
        return {'author' : {
                'author_id' : row[0],
                'author_username' : row[1]
            }}

    #Author is not stored in DB thus data are gathered via Twitter get_user API
    tweet_author = client.get_user(id=tweet_author_id, user_fields = ['verified', 'url', 'public_metrics', 'location', 'description', 'created_at']).data 

    #Store Author to DB and process data and return it to tweet
    sql = "INSERT INTO authors (author_id, author_username, author_name, author_description, author_verified, author_followers_count, author_following_count, author_tweet_count, author_listed_count, author_created_date, author_created_time) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)"
    try:
        cursor.execute(sql, (tweet_author.id,
                                tweet_author.username,
                                tweet_author.name,
                                tweet_author.description,
                                tweet_author.verified,
                                tweet_author.public_metrics.get('followers_count'),
                                tweet_author.public_metrics.get('following_count'),
                                tweet_author.public_metrics.get('tweet_count'),
                                tweet_author.public_metrics.get('listed_count'),
                                '{}-{}-{}'.format(tweet_author.created_at.month,tweet_author.created_at.day, tweet_author.created_at.year),
                                '{}:{}:{}'.format(tweet_author.created_at.hour,tweet_author.created_at.minute, tweet_author.created_at.second)))
        
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
        
    print('Username {} was stored to DB'.format(tweet_author.username))
    tweet_author_processed = process_author_data(tweet_author)
    
    return tweet_author_processed

# Checks if tweet or author is already stored in the DB
def check_existence_by_ID(type, id):
    
    sql = 'SELECT * FROM {}s WHERE {}_id = {}'.format(type, type, id)
    data = None
    
    try:
        cursor.execute(sql)
        data = cursor.fetchone()
        
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    
    return data

# Processes data about the Author from Twitter API and stores returns JSON
def process_author_data(tweet_author):
    
    tweet_author_processed = {
            'author' : {
                'author_id' : tweet_author.id,
                'author_username' : tweet_author.username,
                'author_name' : tweet_author.name,
                'author_description' : tweet_author.description,
                'author_verified' : tweet_author.verified,
                'author_followers_count' : tweet_author.public_metrics.get('followers_count'),
                'author_following_count' : tweet_author.public_metrics.get('following_count'),
                'author_tweet_count' : tweet_author.public_metrics.get('tweet_count'),
                'author_listed_count' : tweet_author.public_metrics.get('listed_count'),
                'author_created_date' : '{}-{}-{}'.format(tweet_author.created_at.month,tweet_author.created_at.day, tweet_author.created_at.year),
                'author_created_time' : '{}:{}:{}'.format(tweet_author.created_at.hour,tweet_author.created_at.minute, tweet_author.created_at.second)
            }
        }
    
    return tweet_author_processed
    

# Compile tweets data from various sources
def process_tweet_data(tweet_data, author_source, author_origin_username, author_origin_id, tweet_type, referenced_tweet_ID):
    
    tweet_data_processed = {
            'tweet' : {
                'tweet_ID' : tweet_data.id,
                'author_source' : author_source.get('author_username'),
                'author_source_id' : author_source.get('author_id'),
                'author_origin' : author_origin_username,
                'author_origin_id' : author_origin_id,
                'search_hashtag' : query,
                'tweet_type' : tweet_type,
                'tweet_text' : tweet_data.text,
                'tweet_language' : tweet_data.lang,
                'geography' : tweet_data.geo,
                'created_date' : '{}-{}-{}'.format(tweet_data.created_at.month,tweet_data.created_at.day, tweet_data.created_at.year),
                'created_time' : '{}:{}:{}'.format(tweet_data.created_at.hour,tweet_data.created_at.minute, tweet_data.created_at.second),
                'tweet_sensitive' : tweet_data.possibly_sensitive,
                'referenced_tweet_id' : referenced_tweet_ID,
                'retweet_count' : tweet_data.public_metrics.get('retweet_count'),
                'reply_count' : tweet_data.public_metrics.get('reply_count'),
                'like_count' : tweet_data.public_metrics.get('like_count'),
                'quote_count' : tweet_data.public_metrics.get('quote_count'),
                'imporession_count' : tweet_data.public_metrics.get('impression_count'),
                'tweet_mentions' : getDataList(tweet_data.entities, 'mentions', 'username'),
                'tweet_annotations' : getDataList(tweet_data.entities, 'annotations', 'normalized_text'),
                'tweet_hashtags' : getDataList(tweet_data.entities, 'hashtags', 'tag'),
                'tweet_url' : getDataList(tweet_data.entities, 'urls', 'url'),
                'tweet_exanded_url' : getDataList(tweet_data.entities, 'urls', 'expanded_url'),
                'tweet_entities' : tweet_data.entities,
                'tweet_attachments' : tweet_data.attachments,
                'tweet_context_annotations' : tweet_data.context_annotations,
                'tweet_source' : tweet_data.source,
                'tweet_withheld' : tweet_data.withheld,
            }
        }
    
    return tweet_data_processed

# Source Tweet or author is a person who retweets, comments or quotes another tweets
# Origin tweet or author is a person who originaly created a tweet
# In case the person is the creator of a new tweet source and origin is a same person thus Source = Origin
def getTweetData(tweet):
     
    #Check if the Tweet ID was already processed or not
    if hasattr(tweet, 'id'):
        row = check_existence_by_ID('tweet', tweet.id)
        if row:
            print('Tweet ID: {} stored to DB'.format(tweet.id))
            return {'tweet' : {
                    'author_source_id' : row[2],
            }}  
    
    if isinstance(tweet, int):
        #Check in DB
        row = check_existence_by_ID('tweet', tweet)
        if row:
            print('Tweet ID: {} stored to DB'.format(tweet))
            return {'tweet' : {
                    'author_source_id' : row[2],
            }}
        
        #In case it does not exist get the Tweet info from Twitter Api
        tweet_data = client.get_tweet(id=tweet, tweet_fields=['attachments','author_id','context_annotations','created_at','entities','geo','id','in_reply_to_user_id','lang','possibly_sensitive','public_metrics','referenced_tweets','source','text','withheld']).data

        #Tweet could be deleted. Twitter API return None 
        if tweet_data is None:
            
            sql = "INSERT INTO tweets (tweet_id) VALUES ({})".format(tweet)
            try:
                cursor.execute(sql)
            except (Exception, psycopg2.DatabaseError) as error:
                print(error)
            
            return None
        
       
    else:
        #Process the tweet that passed in parameter or get from Twitter API (in case of tweets that were referenced)
        tweet_data = tweet

    logging.info('Tweet ID %s', tweet_data.id)
    
    #Get Tweet author by ID
    author_source = getTweetAuthorData(tweet_data.author_id).get('author')
    
    #Is the Tweet referencing (reply, quoted, retweet) another tweet
    if tweet_data.referenced_tweets:
        if tweet_data.referenced_tweets[0]['type'] == 'replied_to':
            tweet_type = 'reply'
        if tweet_data.referenced_tweets[0]['type'] == 'quoted':
            tweet_type = 'quoted'
        if tweet_data.referenced_tweets[0]['type'] == 'retweeted':
            tweet_type = 'retweet'

        #Get the ID from original tweet
        referenced_tweet_ID = tweet_data.referenced_tweets[0]['id']
        tweet_origin = getTweetData(referenced_tweet_ID)
        
        #If the original tweet is deleted there cannot be identified the original tweet author
        if tweet_origin is None:
            author_origin_username = 'Unknown'
            author_origin_id = 0
            
        else:
            author_origin = getTweetAuthorData(tweet_origin.get('tweet').get('author_source_id')).get('author')
            
            author_origin_username = author_origin.get('author_username')
            author_origin_id = author_origin.get('author_id')
            
    else:
        author_origin = author_source
        
        author_origin_username = author_origin.get('author_username') 
        author_origin_id = author_origin.get('author_id')
        
        referenced_tweet_ID = None
        tweet_type = 'tweet'

    sql = "INSERT INTO tweets (tweet_id, author_source, author_source_id, author_origin, author_origin_id, search_hashtag, tweet_type, tweet_text, tweet_language, geography, created_date, created_time, tweet_sensitive, referenced_tweet_id, retweet_count, reply_count, like_count, quote_count, impression_count, tweet_mentions, tweet_annotations, tweet_hashtags, tweet_url, tweet_extended_url, tweet_entities, tweet_attachments, tweet_context_annotations, tweet_source, tweet_witheld) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)"
    try:
        cursor.execute(sql, (tweet_data.id,
                                author_source.get('author_username'),
                                author_source.get('author_id'),
                                author_origin_username,
                                author_origin_id,
                                query,
                                tweet_type,
                                tweet_data.text,
                                tweet_data.lang,
                                json.dumps(tweet_data.geo),
                                '{}-{}-{}'.format(tweet_data.created_at.month,tweet_data.created_at.day, tweet_data.created_at.year),
                                '{}:{}:{}'.format(tweet_data.created_at.hour,tweet_data.created_at.minute, tweet_data.created_at.second),
                                tweet_data.possibly_sensitive,
                                referenced_tweet_ID,
                                tweet_data.public_metrics.get('retweet_count'),
                                tweet_data.public_metrics.get('reply_count'),
                                tweet_data.public_metrics.get('like_count'),
                                tweet_data.public_metrics.get('quote_count'),
                                tweet_data.public_metrics.get('impression_count'),
                                getDataList(tweet_data.entities, 'mentions', 'username'),
                                getDataList(tweet_data.entities, 'annotations', 'normalized_text'),
                                getDataList(tweet_data.entities, 'hashtags', 'tag'),
                                getDataList(tweet_data.entities, 'urls', 'url'),
                                getDataList(tweet_data.entities, 'urls', 'expanded_url'),
                                json.dumps(tweet_data.entities),
                                json.dumps(tweet_data.attachments),
                                json.dumps(tweet_data.context_annotations),
                                json.dumps(tweet_data.source),
                                json.dumps(tweet_data.withheld)))
        
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
        
    print('Tweet ID: {} stored to DB, User: {}'.format(tweet_data.id, author_source.get('author_username')))
    tweet_data_processed = process_tweet_data(tweet_data, author_source, author_origin_username, author_origin_id, tweet_type, referenced_tweet_ID)
        
    global tweet_counter
    tweet_counter += 1

    return tweet_data_processed

def get_last_entry_date():
    sql = 'select created_date, created_time from tweets where created_date is not null ORDER BY created_date DESC, created_time DESC LIMIT 1'
    
    try:
        cursor.execute(sql)
        data = cursor.fetchone()
        
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    
    print(data)
    global start_date
    global start_hour
    
    if data:
        start_date = data[0]
        start_hour = data[1].hour
    else:
        start_hour = 0
        
        

# Connect to Database
def connect_to_DB():
    # connect to the PostgreSQL server
    print('Connecting to the PostgreSQL database...')
    
    global connection
    connection = psycopg2.connect(
        host = config['postgresql']['host'],
        port = config['postgresql']['port'],
        database = config ['postgresql']['database'],
        user = config ['postgresql']['username'],
        password = config['postgresql']['password'],
        sslmode='require')
    
    connection.autocommit = True
    
    global cursor
    cursor = connection.cursor()
    

# def from_to_date_time():
#     global start_hour
    
#     global end_hour
     
#      start_hour
    
#     end_hour = 1
    
     
            
        
# Process all tweets from start date to end date parameter in four hour long intervals based on the search query
def process_datarange():
    
    global tweet_counter
    global start_hour
    
    for processing_date in daterange(start_date,end_date):
            
        while (start_hour < 24):
            
            # global tweet_counter
            tweet_counter = 0
            
            start_timedate = '{}-{}-{}T{}:00:00Z'.format(processing_date.year,processing_date.month,processing_date.day,start_hour)
            if start_hour == 23:
                end_timedate = '{}-{}-{}T{}:59:59Z'.format(processing_date.year,processing_date.month,processing_date.day,start_hour)
            else:
                end_timedate = '{}-{}-{}T{}:59:59Z'.format(processing_date.year,processing_date.month,processing_date.day,start_hour+1)
            
            response = tweepy.Paginator(client.search_all_tweets, query=query, start_time=start_timedate, end_time=end_timedate, tweet_fields=['attachments','author_id','context_annotations','created_at','entities','geo','id','in_reply_to_user_id','lang','possibly_sensitive','public_metrics','referenced_tweets','source','text','withheld'], max_results=100).flatten(300)

            for tweet in response:
                try:
                    if tweet_counter <= max_tweets_per_day / 24:
                        getTweetData(tweet)
                        
                        print('Date: {} Tweet number: {}'.format(start_timedate, tweet_counter))
                        logging.info('Date: %s Tweet number: %s',start_timedate, tweet_counter)
                    
                except (Exception) as error:
                    print(error)
            
            start_hour += 1
            
            # if processing_date == end_date and start_hour >= end_hour:
            #     return
            
        
        start_hour = 0
                
        
# Running the script and storing data in Postgres DB
def run_DB():
    
    try:
        connect_to_DB()
        # from_to_date_time()
        get_last_entry_date()
        process_datarange()
        
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    
    finally:
        if connection is not None:
            connection.close()
            print('Database connection closed.')
            
def main():
    run_DB()
  
if __name__== "__main__":
  main()
    
