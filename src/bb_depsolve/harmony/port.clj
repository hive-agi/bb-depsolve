(ns bb-depsolve.harmony.port
  "The ports this subsystem needs from the world. Three, not one.

   Each names a single role, so a caller — and a test double — holds only the
   capability it actually uses: the pipeline reads jars and never resolves
   coordinates, the row check resolves coordinates and never shells out, and
   only the project check needs a classpath. A single IArtifactStore would
   force every double to implement all three.")

(defprotocol IJarReader
  "Reads the inside of a jar."
  (entry-names [this jar]
    "=> the names of every entry in JAR, or nil when it cannot be opened.")
  (class-texts [this jar]
    "=> a seq of [entry-name text] for JAR's class entries, where text is the
     ISO-8859-1 view of the bytes. Lazy or eager is the adapter's choice; the
     caller consumes it once."))

(defprotocol IArtifactResolver
  "Turns a coordinate into a local jar path. Two methods, because \"do we
   already have this?\" and \"get me this\" are different questions with
   different costs, and the triage that decides whether a check is worth doing
   may only ask the cheap one."
  (local-jar [this lib version]
    "=> a local path to LIB at VERSION if it is ALREADY here, else nil. Never
     fetches.")
  (resolve-jar [this lib version]
    "=> a local path to LIB at VERSION, fetching it if needed, or nil when it
     cannot be had. A version about to be PINNED is one nobody has run yet, so
     a cache miss is the normal case, not an error."))

(defprotocol IClasspathSource
  "Answers what a project's classpath actually is."
  (project-jars [this project-dir]
    "=> the jars on PROJECT-DIR's committed classpath, or nil when the basis
     cannot be resolved. nil is a blind read, NOT an empty classpath."))
