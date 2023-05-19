#!/bin/bash

UTXO="e32f63728a9cc76e4b5f78c2d08d29a3746ebb9237912655126064a206add94a#1"
TRESORYADDR="addr_test1vr2te5ydmsp9wq8ekp9xwgpdvku8zcwyj5dlmc3yay2n3xcf6ppjj"
SEND="123000000"
PATHSKEY="/home/cardano/cardaNode/testnet/keys/tresory.skey"

CONTRACTADDR=$(cardano-cli address build --payment-script-file ../contract/AdaLotto.plutus $TESTNET)
echo "address of contract is : $CONTRACTADDR"
echo "utxo : $UTXO"
echo "Change go to  : $TRESORYADDR"
echo "How much i send : $SEND"
echo "Path to skey : $PATHSKEY"


read -t 25 -n 1 -s -r -p $'Press any key to continue ...\n'

cardano-cli transaction build \
--babbage-era \
--tx-in $UTXO \
--tx-out $CONTRACTADDR+$SEND \
--tx-out-inline-datum-file "../datum/adaLottoDatum.json" \
--change-address $TRESORYADDR \
--out-file ./txTmp/tx.draft $TESTNET



cardano-cli transaction sign \
--tx-body-file ./txTmp/tx.draft \
--signing-key-file $PATHSKEY \
--out-file ./txTmp/tx.signed $TESTNET

cardano-cli transaction submit \
--tx-file ./txTmp/tx.signed $TESTNET