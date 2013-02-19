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
  "Ensembl species from name or keyword"
  [species-name]
  (or (.getSpeciesByEnsemblName *registry* (name species-name))
      (.getSpeciesByAlias *registry* (name species-name))))

(defn list-chromosomes
  [species-name]
  (map #(.getChromosomeName %) (vals (.getChromosomes (species species-name)))))

(defn chromosome
  ([species-name chromosome-name]
     (.getChromosomeByName (species species-name) chromosome-name))
  ([species-name chromosome-name ens-version]
     (.getChromosomeByName (species species-name) chromosome-name (str ens-version))))

(defn genes-on-region
  ([species-name chromosome-name begin end]
     (genes-on-region (chromosome species-name chromosome-name) begin end))
  ([chromosome begin end]
     (.getGenesOnRegion chromosome (Integer. begin) (Integer. end)))
  ([chromosome pos]
     (genes-on-region chromosome pos pos)))

(defn variations-on-region
  ([species-name chromosome-name begin end]
     (variations-on-region (chromosome species-name chromosome-name) begin end))
  ([chromosome begin end]
     (.getVariationsOnRegion chromosome (Integer. begin) (Integer. end)))
  ([chromosome pos]
     (variations-on-region chromosome pos pos)))

(defn gene
  ([species-name gene-stable-id]
     (.getGeneByStableID (species species-name) gene-stable-id))
  ([species-name gene-stable-id ens-version]
     (.getGeneByStableID (species species-name) gene-stable-id (str ens-version))))

(defn transcript
  "Get transcript by stable ID"
  ([species-name transcript-stable-id]
     (.getTranscriptByStableID (species species-name) transcript-stable-id))
  ([species-name transcript-stable-id ens-version]
     (.getTranscriptByStableID (species species-name) transcript-stable-id (str ens-version))))

(defn transcript-strand
  "Strand of transcript."
  [transcript]
  (-> transcript (.getChromosomeMapping) (.getTargetCoordinates) (.getStrand)))

(defn coords-vec
  "Return a vector of coordinates [chr start end strand]."
  [coords]
  ((juxt (memfn getStart) (memfn getEnd) (memfn getStrandInt)) coords))

(defn transcript-coords
  "Genomic coordinates of transcript start/stop/strand."
  [transcript]
  (-> transcript (.getChromosomeMapping) (.getTargetCoordinates)))

(defn transcript-chrom
  "Genomic coordinates of transcript start/stop/strand."
  [transcript]
  (-> transcript (.getChromosomeMapping) (.getTarget)))

;;; Strand predicates
(defn strand-int+?
  [strand]
  (= 1 strand))

(defn strand-int-?
  [strand]
  (= -1 strand))

(defn strand+?
  [strand]
  (= uk.ac.roslin.ensembl.model.Coordinate$Strand/FORWARD_STRAND strand))

(defn strand-?
  [strand]
  (= uk.ac.roslin.ensembl.model.Coordinate$Strand/REVERSE_STRAND strand))

;;; Translation 
(defn transcript-translation
  "Returns the canonical translation for this transcript (if any)."
  [transcript]
  (.getCanonicalTranslation transcript))

(defn protein-sequence
  "Returns the protein sequence for this translation."
  [translation]
  (.getProteinSequence translation))

(defn aa->chromosome
  "Convert AA position to chromosome location"
  [translation pos]
  (.getChromosomePositionFromAA translation (Integer. pos)))

(defn aa<-chromosome
  "Convert AA position from chromosome location"
  [translation pos]
  (.getAAPositionFromChromosome translation (Integer. pos)))

(defn cds<-chromosome
  "Get position relative to CDS from chromosome location."
  [translation pos]
  (.getBasePositionFromChromosome translation (Integer. pos)))

(defn cds->chromosome
  "Get chromosome location from position relative to CDS."
  [translation pos]
  (.getChromosomePositionFromBASE translation (Integer. pos)))

(defn cds-length
  [translation]
  (.getLength translation))

(defn transcript<-aa
  "Codon coordinates of amino acid relative to processed transcript."
  [translation aa-pos]
  (.getProcessedTranscriptPositionFromAA translation (Integer. aa-pos)))

(defn transcript-cds-start-coord
  "Start codon coordinate relative to processed transcript."
  [translation]
  (transcript<-aa translation  1))

(defn transcript-cds-start
  "Start codon position relative to processed transcript."
  [translation]
  (.getStart (transcript-cds-start-coord translation)))

(defn cds-dna
  "Return BioJava DNASequence object for translated (CDS) region."
  [translation]
  (.getTranslatedSequence translation))

(defn aa-dna
  "Return DNA from CDS for amino acid position."
  [translation aa-pos]
  (let [pos (cds<-aa aa-pos)]
    (subs (str (cds-dna translation)) (dec pos) (+ pos 2))))

(defn codon-dna
  "DNA of codon at chromosome position chrom-pos in translation."
  [translation chrom-pos]
  (aa-dna translation (aa<-chromosome translation chrom-pos)))

(defn cds-length
  "CDS length in processed transcript (including start/stop)."
  [translation]
  (.getLength translation))

(defn str-cds-dna
  "Return string of translated (CDS) region."
  [translation]
  (.getSequenceAsString (cds-dna translation)))

(defn seq-cds-dna
  "Return seq of BioJava NucleotideCompound for translated (CDS) region."
  [translation]
  (seq (cds-dna translation)))
  
(defn aa-length
  "Amino acid length (including start/stop)."
  [translation]
  (/ (cds-length translation) 3))

(defn ccds-id
  [transcript]
  (.getCcdsID transcript))

(defn highest-ensembl-version
  "Return the highest ensembl schema version"
  []
  (.getHighestEnsemblSchemaVersion *registry*))

(defn ensembl-versions
  "Return a seq of ensembl schema versions"
  []
  (seq (.getKnownSchemaVersions *registry*)))

(defn database
  "Get a single species core database version (or the most recent)
   Usage:
    (get-database \"human\")
    (get-database \"human\" 70)
    (get-database \"human\" \"70\")"
  ([species-name]
     (database species-name (highest-ensembl-version)))
  ([species-name ens-version]
     (.getDatabase *registry* species-name (str ens-version))))

(defn dna-complement
  "Complement of a org.biojava3.core.sequence.DNASequence"
  [dna-seq]
  (-> dna-seq (.getComplement) (.getViewedSequence)))


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
