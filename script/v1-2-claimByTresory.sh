#!/bin/bash
UTXOCONTRACT="e32f63728a9cc76e4b5f78c2d08d29a3746ebb9237912655126064a206add94a#0"
#MYADDR="addr_test1vzr8wv8vdl9uhqwwu76zgsqv2nq7enls8t7nd06e56glh2qkz0nd3"
TRESORYADDR="addr_test1vr2te5ydmsp9wq8ekp9xwgpdvku8zcwyj5dlmc3yay2n3xcf6ppjj" 
COLLAT="fe9535975945fd8b80e8def5cb21ec0442c1621c23b312a56f952df650351992#0"
PKH="d4bcd08ddc025700f9b04a67202d65b87161c4951bfde224e915389b"
ADALOT="100000000"
PATHSKEY="/home/cardano/cardaNode/testnet/keys/tresory.skey"

cardano-cli query protocol-parameters --out-file ../protocol/protocol.params $TESTNET
SLOT=$(cardano-cli query tip $TESTNET | jq .slot)
echo $SLOT
cardano-cli transaction build \
  --babbage-era \
  --tx-in $UTXOCONTRACT\
  --tx-in-script-file ../contract/AdaLotto.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file ../redeemer/unit.json \
  --required-signer-hash $PKH \
  --tx-in-collateral $COLLAT \
  --tx-out $TRESORYADDR+$ADALOT \
  --change-address $TRESORYADDR \
  --protocol-params-file ../protocol/protocol.params \
  --invalid-before $SLOT \
  --out-file ./txTmp/tx.draft $TESTNET


cardano-cli transaction sign \
--tx-body-file ./txTmp/tx.draft \
--signing-key-file $PATHSKEY \
--out-file ./txTmp/tx.signed $TESTNET

cardano-cli transaction submit \
--tx-file ./txTmp/tx.signed $TESTNET