(ns org.bioclojure.bio.ensembl.config)

(defn assembly-exception-type
  [t]
  (uk.ac.roslin.ensembl.config.AssemblyExceptionType/getType
   (.toUpperCase (name t))))

(defn data-source
  [ds]
  (uk.ac.roslin.ensembl.config.DBConnection$DataSource/valueOf
   (.toUpperCase (name ds))))

(defn ensembl-db-type
  [t]
  (uk.ac.roslin.ensembl.config.EnsemblDBType/getDBTypeForName (name t)))
