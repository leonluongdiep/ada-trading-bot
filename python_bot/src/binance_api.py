# trading/binance_api.py
from binance.client import Client
import os
from dotenv import load_dotenv

# .env Datei laden (API_KEY und API_SECRET)
load_dotenv()

api_key = os.getenv("BINANCE_API_KEY")
api_secret = os.getenv("BINANCE_API_SECRET")

client = Client(api_key, api_secret)

def get_account_balance():
    balances = client.get_account()['balances']
    return [b for b in balances if float(b['free']) > 0 or float(b['locked']) > 0]
