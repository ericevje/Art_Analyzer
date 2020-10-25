import urllib.request
import pandas as pd

df = pd.read_csv("WikiArt-Emotions-Ag4.csv")
image = urllib.request.URLopener()
# for row in range(3448, len(df["Image URL"])):
#     url_secure = df["Image URL"][row]
#     filename = df["ID"][row]
#     url = url_secure.replace("https", "http")
#     print(url)
#     image.retrieve(url, "{}.jpg".format(filename))

url_secure = df["Image URL"]["ID"=="58c6237dedc2c9c7dc0de1ae"]
filename = "58c6237dedc2c9c7dc0de1ae"
url = url_secure.replace("https", "http")
print(url)
image.retrieve(url, "{}.jpg".format(filename))
