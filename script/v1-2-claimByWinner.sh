#!/bin/bash

UTXOCONTRACT="f49144f771808e6fb98da3e0be1a4f2b69895d7ec41d7d98adbdb67fc5f02087#0"
#MYADDR="addr_test1vzr8wv8vdl9uhqwwu76zgsqv2nq7enls8t7nd06e56glh2qkz0nd3"
WINNERADDR="addr_test1vzr8wv8vdl9uhqwwu76zgsqv2nq7enls8t7nd06e56glh2qkz0nd3" 
COLLAT="afbb2acfdb1600a3052a1b1bdab9081b4b95ff286dd2658caa4352126475c2f3#1"
PKH="867730ec6fcbcb81cee7b424400c54c1eccff03afd36bf59a691fba8"
ADALOT="100000000"
PATHSKEY="/home/cardano/cardaNode/testnet/keys/addr1.skey"


cardano-cli query protocol-parameters --out-file ../protocol/protocol.params $TESTNET
SLOT=$(cardano-cli query tip $TESTNET | jq .slot)
echo $SLOT

cardano-cli transaction build \
  --babbage-era \
  --tx-in $UTXOCONTRACT \
  --tx-in-script-file ../contract/AdaLotto.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file ../redeemer/unit.json \
  --required-signer-hash $PKH \
  --tx-in-collateral $COLLAT \
  --tx-out $WINNERADDR+$ADALOT \
  --change-address $WINNERADDR \
  --protocol-params-file ../protocol/protocol.params \
  --invalid-before $SLOT \
  --out-file ./txTmp/tx.draft $TESTNET


cardano-cli transaction sign \
--tx-body-file ./txTmp/tx.draft \
--signing-key-file $PATHSKEY \
--out-file ./txTmp/tx.signed $TESTNET

cardano-cli transaction submit \
--tx-file ./txTmp/tx.signed $TESTNET