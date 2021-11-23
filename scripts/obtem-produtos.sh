#/bin/sh
curl -H "Content-Type: application/json" --data @obtem-produtos.json --request POST http://localhost:3000/produtos
