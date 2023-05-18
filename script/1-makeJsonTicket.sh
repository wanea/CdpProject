#/bin/bash


#this ticket need to be create by offchain app like website
TICKET="[1,8,11,27,34,42]" # List of number between 1 to 49 

#this is the format checked by blockfrost in the contract
#light data  for the blockchain
echo "{\"713705\": {\"ticket\": $TICKET}}" > ticket.json