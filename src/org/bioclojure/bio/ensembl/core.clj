(ns org.bioclojure.bio.ensembl.core
  (:require [clojure.java.io :as io]
            [clojure.string :refer (upper-case)]
            [org.bioclojure.bio.ensembl.config :only (data-source)])
  (:import [uk.ac.roslin.ensembl.config RegistryConfiguration DBConnection$DataSource]
           [uk.ac.roslin.ensembl.dao.database DBRegistry DBSpecies]
           [uk.ac.roslin.ensembl.model Coordinate StableID]
           [uk.ac.roslin.ensembl.model.database Registry]
           [uk.ac.roslin.ensembl.model.core
            DNASequence Feature Gene Species Transcript Translation Chromosome]
           [uk.ac.roslin.ensembl.model.variation Variation]
           [uk.ac.roslin.ensembl.datasourceaware.core DATranslation DAGene DATranscript]))

(defonce ^:dynamic ^Registry *registry* nil)

(defmacro with-registry
  [registry & body]
  `(binding [*registry* ~registry] ~@body))

(defn set-registry!
  [registry]
  (alter-var-root #'*registry*
                  (constantly registry)
                  (when (thread-bound? #'*registry*)
                    (set! *registry* registry))))

(defn local-config
  [conf-file]
  (DBRegistry. (doto (RegistryConfiguration.)
                 (.setDBByFile (io/file conf-file)))
               true))

(defn data-source
  [ds]
  (DBConnection$DataSource/valueOf
   (.toUpperCase (name ds))))

(defn registry
  [ds]
  (DBRegistry. (data-source ds)))

(defn ensembl-version
  "Return current Ensembl version of connected database registry for species-name."
  [species-name]
  (-> *registry* (.getDatabase (name species-name)) .getDBVersion ))


(defn- list-species-transform
  [style]
  (get {:binomial #(.getSpeciesBinomial ^Species %)
        :common   #(.getCommonName ^Species %)
        :compara  #(.getComparaName ^Species % (ensembl-version %))
        :database #(.getDatabaseStyleName ^Species %)
        :display  #(.getDisplayName ^Species %)
        :short    #(.getShortName ^Species %)}
       style
       identity))

(defn list-species
  [& [style]]
  (map (list-species-transform style) (.getSpecies *registry*)))

(defn species
  "Ensembl species from name or keyword"
  ^Species
  [species-name]
  (or (.getSpeciesByEnsemblName *registry* (name species-name))
      (.getSpeciesByAlias *registry* (name species-name))))

(defn species-version
  "Return genome assembly version of species."
  [species-name]
  (.getAssemblyName (species species-name) (ensembl-version species-name)))

(defn list-chromosomes
  [species-name]
  (map #(.getChromosomeName ^Chromosome %) (vals (.getChromosomes (species species-name)))))

(defn chromosome
  ^Chromosome
  ([species-name chromosome-name]
     (.getChromosomeByName (species species-name) chromosome-name))
  ([species-name chromosome-name ens-version]
     (.getChromosomeByName (species species-name) chromosome-name (str ens-version))))

(defn genes-on-region
  ([species-name chromosome-name begin end]
     (genes-on-region (chromosome species-name chromosome-name) begin end))
  ([^Chromosome chromosome begin end]
     (.getGenesOnRegion chromosome (int begin) (int end)))
  ([chromosome pos]
     (genes-on-region chromosome pos pos)))

(defn variations-on-region
  ([species-name chromosome-name begin end]
     (variations-on-region (chromosome species-name chromosome-name) begin end))
  ([^Chromosome chromosome begin end]
     (.getVariationsOnRegion chromosome (int begin) (int end)))
  ([chromosome pos]
     (variations-on-region chromosome pos pos)))

(declare highest-ensembl-version)
(defn gene
  ([species-name gene-stable-id]
     (.getGeneByStableID (species species-name) gene-stable-id))
  ([species-name gene-stable-id ens-version]
     (.getGeneByStableID (species species-name) gene-stable-id (str ens-version))))

(defn gene-transcripts
  ([^Gene gene]
     (.getTranscripts gene)))

(defn transcript
  "Get transcript by stable ID"
  ([species-name transcript-stable-id]
     (.getTranscriptByStableID (species species-name) transcript-stable-id))
  ([species-name transcript-stable-id ens-version]
     (.getTranscriptByStableID (species species-name) transcript-stable-id (str ens-version))))

(defn exons
  [transcript]
  (.getExons transcript))

(defn gene-stable-id
  [^DAGene gene]
  (.getStableID gene))

(defn gene-name
  [^DAGene gene]
  (.getDisplayName gene))

(defn gene-name->genes
  "Retrieve list of Ensembl genes from gene names."
  ([species-name gene-name]
     (gene-name->genes species-name gene-name (highest-ensembl-version)))
  ([species-name gene-name ens-version]
     (->> (.getGenesForExactName (species species-name) gene-name (str ens-version))
          (map gene-stable-id)
          (filter #(.startsWith % "ENS"))
          (map #(gene species-name % ens-version)))))

(defn gene-description
  [^Gene gene]
  (.getDescription gene))

(defn transcript-stable-id
  [^DATranscript transcript]
  (.getStableID transcript))

(defn transcript-strand
  "Strand of transcript."
  [^Transcript transcript]
  (-> transcript (.getChromosomeMapping) (.getTargetCoordinates) (.getStrand)))

(defn coords-vec
  "Return a vector of coordinates [chr start end strand]."
  [^Coordinate coords]
  ((juxt #(.getStart ^Coordinate %) #(.getEnd ^Coordinate %) #(.getStrandInt ^Coordinate %)) coords))

(defn coords-map
  "Return a map of coordinate information with chromosome"
  [x]
  (let [mapping (.getChromosomeMapping x)
        coords (.getTargetCoordinates mapping)]
    {:chr (.getChromosomeName (.getTarget mapping))
     :start (.getStart coords) :end (.getEnd coords)
     :strand (.getStrandInt coords)}))

(defn transcript->exon-coords
  "Convert a transcript into a map of exon coordinates :chr :start :end :strand"
  [transcript]
  (map coords-map (exons transcript)))

(defn transcript-coords
  "Genomic coordinates of transcript start/stop/strand."
  [^Transcript transcript]
  (-> transcript (.getChromosomeMapping) (.getTargetCoordinates)))

(defn transcript-chrom
  "Genomic coordinates of transcript start/stop/strand."
  [^Transcript transcript]
  (-> transcript (.getChromosomeMapping) (.getTarget)))

(defn transcript-biotype
  [^DATranscript transcript]
  (.getBiotype transcript))

(defn transcript-gene
  [^Transcript transcript]
  (.getGene transcript))

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
(defn transcript-canonical-translation
  "Returns the canonical translation for this transcript (if any)."
  [^Transcript transcript]
  (.getCanonicalTranslation transcript))

(defn transcript-translations
  "Returns the translations for this transcript (if any)."
  [^Transcript transcript]
  (.getTranslations transcript))

(defn protein-sequence
  "Returns the protein sequence for this translation."
  [^Translation translation]
  (.getProteinSequence translation))

(defn aa->chromosome
  "Convert AA position to chromosome location"
  [^DATranslation translation pos]
  (.getChromosomePositionFromAA translation (int pos)))

(defn aa<-chromosome
  "Convert AA position from chromosome location"
  [^DATranslation translation pos]
  (.getAAPositionFromChromosome translation (int pos)))

(defn cds<-chromosome
  "Get position relative to CDS from chromosome location."
  [^DATranslation translation pos]
  (.getBasePositionFromChromosome translation (int pos)))

(defn cds->chromosome
  "Get chromosome location from position relative to CDS."
  [^DATranslation translation pos]
  (.getChromosomePositionFromBASE translation (int pos)))

(defn transcript<-aa
  "Codon coordinates of amino acid relative to processed transcript."
  ^Coordinate
  [^DATranslation translation aa-pos]
  (.getProcessedTranscriptPositionFromAA translation (int aa-pos)))

(defn transcript-cds-start-coord
  "Start codon coordinate relative to processed transcript."
  ^Coordinate
  [translation]
  (transcript<-aa translation  1))

(defn transcript-cds-start
  "Start codon position relative to processed transcript."
  [^DATranslation translation]
  (.getStart (transcript-cds-start-coord translation)))

(defn cds-dna
  "Return BioJava DNASequence object for translated (CDS) region."
  ^DNASequence
  [^DATranslation translation]
  (.getTranslatedSequence translation))

(defn cds<-aa
  "Codon start position of amino acid relative to CDS start (1-based)."
  [aa-pos]
  (inc (* 3 (dec aa-pos))))

(defn aa-dna
  "Return DNA from CDS for amino acid position."
  [translation aa-pos]
  (let [pos (cds<-aa aa-pos)]
    (when-let [dna (cds-dna translation)]
      (subs (str dna) (dec pos) (+ pos 2)))))

(defn codon-dna
  "DNA of codon at chromosome position chrom-pos in translation."
  [translation chrom-pos]
  (aa-dna translation (aa<-chromosome translation chrom-pos)))

(defn cds-length
  "CDS length in processed transcript (including start/stop)."
  [^DATranslation translation]
  (.getLength translation))

(defn str-cds-dna
  "Return string of translated (CDS) region."
  ^String
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
  [^Transcript transcript]
  (.getCcdsID transcript))

(defn highest-ensembl-version
  "Return the highest ensembl schema version"
  []
  (.getHighestEnsemblSchemaVersion *registry*))

(defn ensembl-versions
  "Return a seq of ensembl schema versions in the registry."
  []
  (seq (.getKnownSchemaVersions ^DBRegistry *registry*)))

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
  [^org.biojava3.core.sequence.DNASequence dna-seq]
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

  (ensembl-versions)

  ;; local Ensembl connections - for file format see:
  ;;   http://jensembl.svn.sourceforge.net/viewvc/jensembl/trunk/EnsemblTest/src/main/resources/
  (set-registry! (local-config "example_local_configuration.properties")))
