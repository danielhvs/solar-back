#/bin/sh
rm ../resources/barras*
rm 12345678910.pdf
curl -H "Content-Type: application/json" --data @body.json --request POST http://localhost:3000/orcamento --output 12345678910.pdf
evince 12345678910.pdf
