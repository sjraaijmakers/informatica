import csv

producten = []
with open('Voorraadlijst.csv') as csvfile:
     reader = csv.DictReader(csvfile)
     producten = list(reader)

hoogsteprijs = 0
for p in producten:
    if p["Prijs"] > hoogsteprijs:
        hoogsteprijs = p["Prijs"]

for p in producten:
    if p["Prijs"] == hoogsteprijs:
        print p
        break

print max(producten, key=Prijs.get)
