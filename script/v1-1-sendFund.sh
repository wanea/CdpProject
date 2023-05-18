#!/bin/bash

UTXO1="a3ba65349ac4803515c74eaec0d4e7f07c76647db9805d6ea7f876c84fc47f4a#1"
MYADDR="addr_test1vzr8wv8vdl9uhqwwu76zgsqv2nq7enls8t7nd06e56glh2qkz0nd3"
#TRESORYADDR="addr_test1vpklwt8xluxhym355tur82kd9xfx0fmnxd0qx203xsytdvgjn4akl" 
CONTRACTPATH="./contract/AdaLotto.plutus"
SEND="100000000"
PATHSKEY="/home/cardano/cardaNode/testnet/keys/addr1.skey"

CONTRACTADDR=$(cardano-cli address build --payment-script-file ../contract/AdaLotto.plutus $TESTNET)
echo "address of contract is : $CONTRACTADDR"
read -t 5 -n 1 -s -r -p $'Press any key to continue ...\n'
echo "utxo : $UTXO"
echo "Change go to  : $MYADDR"
echo "How much i send : $SEND"
echo "Path to skey : $PATHSKEY"


read -t 25 -n 1 -s -r -p $'Press any key to continue ...\n'

cardano-cli transaction build \
  --babbage-era \
  --tx-in $UTXO\
  --tx-out $CONTRACADDR+$SEND \
  --tx-out-inline-datum-file ./datum/adaLottoDatum.json \
  --change-address $MYADDR \
  --out-file ./txTmp/tx.draft $TESTNET

cardano-cli transaction sign \
--tx-body-file ./txTmp/tx.draft \
--signing-key-file $PATHSKEY \
--out-file ./txTmp/tx.signed $TESTNET

cardano-cli transaction submit \
--tx-file ./txTmp/tx.signed $TESTNET