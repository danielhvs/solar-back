#/bin/sh
curl -H "Content-Type: application/json" --data @obtem-transformadores.json --request POST http://localhost:3000/transformadores
