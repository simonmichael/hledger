text = '''"Date","Remarque","Montant"
"2012/3/22","DÉPÔT","50.00"
"2012/3/23","VIREMENT VERS ÉPARGNE","-10.00"
"2012/3/24","CAFÉ — €20 REÇU","-20.00"'''

# Create UTF-8 file
with open('sample-fr.utf8.csv', 'wb') as f:
    f.write(text.encode('utf-8'))

# Create ISO-8859-1 file, replacing unsupported characters
with open('sample-fr.iso8859-1.csv', 'wb') as f:
    f.write(text.encode('iso-8859-1', errors='replace'))

# Create Windows-1252 file
with open('sample-fr.cp1252.csv', 'wb') as f:
    f.write(text.encode('cp1252', errors='replace'))
