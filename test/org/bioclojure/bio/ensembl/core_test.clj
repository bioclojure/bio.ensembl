(ns org.bioclojure.bio.ensembl.core-test
  (:require [clojure.test :refer :all]
            [org.bioclojure.bio.ensembl.core :as ens]))

(deftest human-exon-test
  (testing "Retrieve exon coordinates from human Ensembl database."
    (ens/with-registry (ens/registry :ensembldb)
      (let [species "human"
            gene (first (ens/gene-name->genes species "BRCA2"))
            ts (ens/gene-transcripts gene)
            coords (map ens/transcript->exon-coords ts)]
        (doseq [c coords]
          (println c))))))
