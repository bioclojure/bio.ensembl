(defproject org.bioclojure/bio.ensembl "0.1.0-SNAPSHOT"
  :description "Clojure API for Ensembl data access"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["jensembl" {:url "http://jensembl.sourceforge.net/m2-repo" :checksum :ignore :snapshots false }]
                 ["biojava" {:url "http://www.biojava.org/download/maven/" :checksum :ignore :snapshots false }]]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [uk.ac.roslin/ensembl-data-access "1.13"]
                 [uk.ac.roslin/ensembl-config "1.68"]])
