import requests
url = "https://rqzwlxn788.execute-api.us-east-1.amazonaws.com/test/predict"

data = {"url":"https://raw.githubusercontent.com/alexeygrigorev/clothing-dataset-small/master/test/pants/4aabd82c-82e1-4181-a84d-d0c6e550d26d.jpg"}

result = requests.post(url,json=data).json()
print(result)