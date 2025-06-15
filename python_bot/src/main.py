from binance_api import get_account_balance

portfolio = get_account_balance()

print("\n📊 Dein Binance-Portfolio:")
for asset in portfolio:
    print(f"{asset['asset']}: {asset['free']} frei, {asset['locked']} gesperrt")
