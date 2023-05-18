#!/bin/bash

UTXO1="a3ba65349ac4803515c74eaec0d4e7f07c76647db9805d6ea7f876c84fc47f4a#1"
MYADDR="addr_test1vzr8wv8vdl9uhqwwu76zgsqv2nq7enls8t7nd06e56glh2qkz0nd3"
#TRESORYADDR="addr_test1vpklwt8xluxhym355tur82kd9xfx0fmnxd0qx203xsytdvgjn4akl" 
COLLAT="4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0"
PKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"




cardano-cli transaction build \
  --babbage-era \
  --tx-in $utxoin \
  --tx-in-script-file ../contract/AdaLotto.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file ../redeemer/unit.json \
  --required-signer-hash $PKH \
  --tx-in-collateral $COLLAT \
  --tx-out $MYADDR+$output \
  --change-address $MYADDR \
  --protocol-params-file ../protocol/protocol.params \
  --out-file ./txTmp/tx.draft $TESTNET


cardano-cli transaction sign \
--tx-body-file ./txTmp/tx.draft \
--signing-key-file $PATHSKEY \
--out-file ./txTmp/tx.signed $TESTNET

cardano-cli transaction submit \
--tx-file ./txTmp/tx.signed $TESTNET