import streamlit as st
import requests
import urllib.request
from PIL import Image
import regex as re

st.title("Fashion Classification App")
st.write("This web app uses a keras trained model to classify an image into one of 10 Classes")

image_url =st.text_input("Enter the url of the image")

url = "https://rqzwlxn788.execute-api.us-east-1.amazonaws.com/test/predict"

data = {"url":image_url}

st.write("Here is a sneek peak of your uploaded Image:")
# Regex for a url ending with jpg
regex_jpg = r'(.*)\.jpg$'

# Regex for a url ending with png
regex_png = r'(.*)\.png$'
if image_url is not None:
    
    if re.match(regex_jpg,image_url):
        urllib.request.urlretrieve(image_url,"img.jpg")

        img = Image.open("img.jpg")
        st.image(img)
    elif re.match(regex_png,image_url):
        urllib.request.urlretrieve(image_url,"img.png")
        img = Image.open("img.png")
        st.image(img)

    result = requests.post(url,json=data).json()
    print(result)
        
    maximu = [key for key,value in result.items() if float(value) == max(result.values())][0]

    result = f"You have uploaded an image of {maximu}"
    st.markdown("### Prediction")
    st.write(result)