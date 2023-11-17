#!/bin/bash

# Step 1: Download the file using wget
curl -o NC_000913.faa https://ftp.ncbi.nlm.nih.gov/genomes/archive/old_refseq/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid57779/NC_000913.faa

# Step 2: Count the number of sequences in the strain
Np=$(grep -c "^>" NC_000913.faa)

# if we use the provided gz file directly we will change the command to 
# Np=$(gunzip -c NC_000913.faa.gz | grep -c "^>") 

echo "Number of sequences : $Np"

# Step 3: Calculate the total number of amino acids
sum=$(grep -v "^>" NC_000913.faa| awk '{ sum += length } END { print sum }')

# if we use the provided gz file directly we will change the command to 
# sum=$(gunzip -c NC_000913.faa.gz | grep -v "^>" | awk '{ sum += length } END { print sum }')

echo "Total number of amino acids: $sum"

# Step 4: Calculate the average length of amino acids per protein sequence
average=$((sum / Np))
echo "Average length of protein in the strain of E. coli: $average"
