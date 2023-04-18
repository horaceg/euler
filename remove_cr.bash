#! /bin/bash

# Remove carriage return characters made on windows for linux

ls *.ml | xargs -I _ sh -c "sed 's/\r$//' _ > _b"
rm *.ml

for file in *.mlb; do 
    mv -- "$file" "${file%.mlb}.ml"
done
