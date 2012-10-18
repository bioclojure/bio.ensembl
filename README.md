# bio.ensembl

An experimental Clojure API wrapping the excellent Java Ensembl
library JEnsembl, <http://jensembl.sourceforge.net/>.

This is *alpha* software: it is incomplete and the API is subject to
change.

Please drop me a line if you would like to help with this effort or
have suggestions for how the API should shape up.

## Usage

    (use 'org.bioclojure.bio.ensembl.core)
 
    (def ensreg (registry :ensembldb))

    (with-registry ensreg
      (list-species)
      (chromosome "human" "20")
      (gene "human" "ENSG00000153551"))

Alternatively, set the registry for your session:
      
    (set-registry! ensreg)

    (list-species)

    (list-species :binomial)

    (chromosome "human" "20")

    (list-chromosomes "human")

    (genes-on-region "human" "20" 1 100000)

    (genes-on-region (chromosome "human" "20") 1 100000)

## License

Copyright (C) 2012 Ray Miller <ray@1729.org.uk>.

Distributed under the Eclipse Public License, the same as Clojure.
