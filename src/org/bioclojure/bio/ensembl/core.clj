(ns org.bioclojure.bio.ensembl.core
  (:use [org.bioclojure.bio.ensembl.config :only (data-source)]
        [clojure.string :only (upper-case)])
  (:import [uk.ac.roslin.ensembl.dao.database DBRegistry DBSpecies]
           [uk.ac.roslin.ensembl.model.core
            DNASequence Feature Gene Species Transcript Translation]
           [uk.ac.roslin.ensembl.model.variation Variation]
           [uk.ac.roslin.ensembl.model Coordinate]))

(def ^:dynamic *registry* nil)

(defmacro with-registry
  [registry & body]
  `(binding [*registry* ~registry] ~@body))

(defn set-registry!
  [registry]
  (alter-var-root #'*registry*
                  (constantly registry)
                  (when (thread-bound? #'*registry*)
                    (set! *registry* registry))))

(defn registry
  [ds]
  (DBRegistry. (data-source ds)))

(defn- list-species-transform
  [style]
  (get {:binomial (memfn getSpeciesBinomial)
        :common   (memfn getCommonName)
        :compara  (memfn getComparaName)
        :database (memfn getDatabaseStyleName)
        :display  (memfn getDisplayName)
        :short    (memfn getShortName)}
       style
       identity))

(defn list-species
  [& [style]]
  (map (list-species-transform style) (.getSpecies *registry*)))

(defn species
  [species-name]
  (or (.getSpeciesByEnsemblName *registry* (name species-name))
      (.getSpeciesByAlias *registry* (name species-name))))

(defn list-chromosomes
  [species-name]
  (map #(.getChromosomeName %) (vals (.getChromosomes (species species-name)))))

(defn chromosome
  [species-name chromosome-name]
  (.getChromosomeByName (species species-name) chromosome-name))

(defn genes-on-region
  ([species-name chromosome-name begin end]
     (genes-on-region (chromosome species-name chromosome-name) begin end))
  ([chromosome begin end]
     (.getGenesOnRegion chromosome (Integer. begin) (Integer. end))))

(defn gene
  [species-name gene-stable-id]
  (.getGeneByStableID (species species-name) gene-stable-id))

(comment

  (def ensreg (registry :ensembldb))

  (with-registry ensreg
    (list-species)
    (chromosome "human" "20")
    (gene "human" "ENSG00000153551"))

  (set-registry! ensreg)

  (list-species)

  (chromosome "human" "20")

  (list-chromosomes "human")

  (genes-on-region "human" "20" 1 100000)

  (genes-on-region (chromosome "human" "20") 1 100000)

  )
