#!/bin/bash

UTXO1="a3ba65349ac4803515c74eaec0d4e7f07c76647db9805d6ea7f876c84fc47f4a#1"
#UTXO2="f605cca5b607e3213a9023394ade09cc2116b6128b9126a5f117cab78a7a120b#0"
#UTXO3="f605cca5b607e3213a9023394ade09cc2116b6128b9126a5f117cab78a7a120b#1"
MYADDR="addr_test1vzr8wv8vdl9uhqwwu76zgsqv2nq7enls8t7nd06e56glh2qkz0nd3"
TRESORYADDR="addr_test1vpklwt8xluxhym355tur82kd9xfx0fmnxd0qx203xsytdvgjn4akl"
SEND="1000000"
PATHSKEY="/home/cardano/cardaNode/testnet/keys/addr1.skey"


echo "utxo : $UTXO"
echo "address : $ADDR"
echo "send : $SEND"
echo "Path to skey : $PATHSKEY"



read -t 5 -n 1 -s -r -p $'Press any key to continue ...\n'


cardano-cli transaction build \
--tx-in $UTXO1 \
--tx-out $TRESORYADDR+$SEND \
--change-address $MYADDR \
--metadata-json-file ./ticket.json \
--out-file tx.draft $TESTNET

cardano-cli transaction sign \
--tx-body-file ./tx.draft \
--signing-key-file $PATHSKEY \
--out-file tx.signed $TESTNET

cardano-cli transaction submit \
--tx-file ./tx.signed $TESTNET