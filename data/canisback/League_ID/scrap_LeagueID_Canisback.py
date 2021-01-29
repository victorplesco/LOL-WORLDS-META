import re
import os
import requests
import urllib.request
import time
from bs4 import BeautifulSoup

# Set the URL to webscrape from
url = "https://canisback.com/leagueId/"

# Connect to the URL
response = requests.get(url)

# Parse HTML and save to BeautifulSoup object
soup = BeautifulSoup(response.text, "html.parser")

# Find date of update of the LeagueID files
update = re.findall(r'\w{2}-\w{3}-\w{4} \w{2}:\w{2}', str(soup.findAll("pre")))
for index, i in enumerate(update):
    tmp = str(i).replace(" ", "_")
    update[index] = tmp.replace("-", "_")

# Defining Remote and Local files
remoteFiles = []
for index, i in enumerate(soup.findAll("a")[1:]):
    remoteFiles.append(str(i["href"]).replace("csv", "") + update[index])
localFiles = []
for index, i in enumerate(os.listdir(path = "./data/metadata/canisback/League_ID/")):
    if re.findall(r'\Aleague', str(i)):
        localFiles.append(str(i))

# Scraping
for index_i, i in enumerate(remoteFiles):
    counter = 0
    if(len(localFiles) == 0): # Used for the first download;
        urllib.request.urlretrieve(url      = ("https://canisback.com/leagueId/" + str(i).split(".")[0] + ".csv"), 
                                   filename = ("./data/metadata/canisback/League_ID/" + remoteFiles[index_i]))
        continue
    else:
        for j in localFiles:   
            counter += 1
            if(str(i).split(".")[0] == str(j).split(".")[0]):
                if(str(i).split(".")[1] == str(j).split(".")[1]):
                    break
                else: # If dates are different, download new and delete old;
                    urllib.request.urlretrieve(url      = ("https://canisback.com/leagueId/" + str(i).split(".")[0] + ".csv"), 
                                               filename = ("./data/metadata/canisback/League_ID/" + remoteFiles[index_i]))
                    os.remove(path = "./data/metadata/canisback/League_ID/" + str(j))
                    break
            else:
                if(counter == len(localFiles)): # If new file, download it;
                    urllib.request.urlretrieve(url      = ("https://canisback.com/leagueId/" + str(i).split(".")[0] + ".csv"), 
                                               filename = ("./data/metadata/canisback/League_ID/" + remoteFiles[index_i]))
                else:
                    continue

#####################################################################################################################################################################
## References #######################################################################################################################################################
#####################################################################################################################################################################

# Python RegEx
# https://www.w3schools.com/python/python_regex.asp

# How to Web Scrape with Python in 4 Minutes
# https://towardsdatascience.com/how-to-web-scrape-with-python-in-4-minutes-bc49186a8460

# A Practical Introduction to Web Scraping in Python
# https://realpython.com/python-web-scraping-practical-introduction/

# Beautiful Soup: Build a Web Scraper With Python
# https://realpython.com/beautiful-soup-web-scraper-python/

# Index of /leagueId/
# https://canisback.com/leagueId/

# Collecting Data
# https://riot-api-libraries.readthedocs.io/en/latest/collectingdata.html