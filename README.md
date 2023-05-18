Project for Cardano developper Program

to try the project 

git clone this address & cabal repl 
no nix-shell needed

the contract is in datum
to build a contract we need to provide :
        * valid signer (pubKeyHash)
        * limit time of contract 
        * an array of number

the valid signer is the person who find the right number of the lottery

the limit of contract is the delay to the winner for execute the contract 

the array is the ticket of lottery (array of 6 number between [1-49])



To take a ticket we need to send specific json file to a specific label
