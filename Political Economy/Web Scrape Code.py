from stem import Signal
from stem.control import Controller

import urllib.request, urllib.error 
import re, time, csv, sys
import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
from collections import Counter
#from ConnectionManager import ConnectionManager

# https://jarroba.com/anonymous-scraping-by-tor-network/
# https://gist.github.com/KhepryQuixote/46cf4f3b999d7f658853

#### Define functions needed to switch IPs ####

class ConnectionManager:
    def __init__(self):
        self.new_ip = "0.0.0.0"
        self.old_ip = "0.0.0.0"
        self.new_identity()

    @classmethod
    def _get_connection(self):
        """
        TOR new connection
        """
        with Controller.from_port(port=9051) as controller:
            controller.authenticate(password="si618w18a2jac92") # should this be hashed? 16:AD04648222A4FC3A60F4000E61B112441C33198C557EBDED5CB64547AC
            controller.signal(Signal.NEWNYM)
            controller.close()

    @classmethod
    def _set_url_proxy(self):
        """
        Request to URL through local proxy
        """
        proxy_support = urllib.ProxyHandler({"http": "127.0.0.1:8118"})
        opener = urllib.request.build_opener(proxy_support)
        urllib.request.install_opener(opener)

    @classmethod
    def request(self, url):
        """
        TOR communication through local proxy
        :param url: web page to parser
        :return: request
        """
        try:
            self._set_url_proxy()
            request = urllib.request.Request(url, None, {
                'User-Agent': "Mozilla/5.0 (X11; Linux x86_64) "
                              "AppleWebKit/535.11 (KHTML, like Gecko) "
                              "Ubuntu/10.10 Chromium/17.0.963.65 "
                              "Chrome/17.0.963.65 Safari/535.11"})
            request = urllib.request.Request(request)
            return request
        except urllib.error.URLError, e: # was urllib2.HTTPError
            return e.message

    def new_identity(self):
        """
        new connection with new IP
        """
        # First Connection
        if self.new_ip == "0.0.0.0":
            self._get_connection()
            self.new_ip = self.request("http://icanhazip.com/").read()
        else:
            self.old_ip = self.new_ip
            self._get_connection()
            self.new_ip = self.request("http://icanhazip.com/").read()

        seg = 0

        # If we get the same ip, we'll wait 5 seconds to request a new IP
        while self.old_ip == self.new_ip:
            time.sleep(5)
            seg += 5
            print ("Waiting to obtain new IP: %s Seconds" % seg)
            self.new_ip = self.request("http://icanhazip.com/").read()

        print ("New connection with IP: %s" % self.new_ip)


#### Retrieve Transcripts ####
cm = ConnectionManager()

ticker = []
link = []
headline = []
text = []


sp_df = pd.read_csv('constituents.csv')
#symbol_list = sp_df['Symbol'].tolist()
symbol_list = ['MMM','AXP','AAPL','BA'] # test run

for symbol in symbol_list:
    print(symbol)
    site = 'http://seekingalpha.com/symbol/'+symbol+'/earnings/transcripts'

    # Obtain new IP to request landing page for each company
    cm.new_identity()
    count = 1

    req = urllib.request.Request(site) #removed headers=hdr
    try:
        page = urllib.request.urlopen(req) 
        soup = BeautifulSoup(page, 'lxml')
        
        # now loop through all of the linked transcripts on the page to find transcripts
        for link in soup.find_all('a'):
            x = link.get('href') # these are all the links on the landing page
            
            if isinstance(x, str):
                # to start, just look for 'transcript' and 'article' in the links.
                # the links we want look like: /article/4132150-3m-mmm-2018-outlook-meeting-conference-transcript
                # with just 'transcript', we get things like: http://seekingalpha.com//earnings/earnings-call-transcripts?part=single
                
                wordlist = ['transcript', 'article']
                if all(x.find(s) >= 0 for s in wordlist):
                    parse_site = 'http://seekingalpha.com/'+x+'?part=single'
                    
                    ticker.append(symbol)
                    link.append(parse_site)

                    # Obtain new IP for each transcript call. Do this every two calls, using "count"
                    if count == 2:
                        cm.new_identity()
                        count = 1
                    else:
                        count += 1

                    parse_req = urllib.request.Request(parse_site) # removed headers=hdr

                    try:
                        parse_page = urllib.request.urlopen(parse_req)
                        parse_soup = BeautifulSoup(parse_page,'lxml')
                                                
                        headline.append(parse_soup.title.string)
                        text.append(parse_soup.text)

                    except urllib.error.URLError as e:
                        print ('Error code:',e.code)
                        headline.append(e.code)
                        text.append(e.code)

    except urllib.error.URLError as e:
        print('Error code:', e.code)


# Combine lists into pandas dataframe, then save as a csv
df = pd.DataFrame({'ticker': ticker, 'link': link, 'title': headline, 'text': text}, columns=['ticker','link','title', 'text'])
df.to_csv("processed_transcripts.csv")
