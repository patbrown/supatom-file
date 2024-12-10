(ns baby.pat.supatom.file
  (:require [babashka.fs :as fs]
            [baby.pat.jes.vt :as vt]
            [baby.pat.supatom]
            [clojure.edn]
            [medley.core]
            [orchestra.core :refer [defn-spec]]))

(defn-spec create-file ::vt/discard
  "Can be called directly like all others, but generally avoided in favor of using higher level functions."
  ([path ::vt/str]
   (let [parent (fs/parent path)
         _ (when (not (fs/exists? parent))
             (fs/create-dirs parent))]
     (fs/create-file path))))

(defn-spec commit! ::vt/discard
  "Commits contents to a file."
  ([path ::vt/str contents ::vt/str]
   (if (fs/exists? path)
     (spit path contents)
     (let [_ (create-file path)]
       (spit path contents)))))

(defn-spec snapshot ::vt/discard
  "Returns a snapshot from a file."
  ([path ::vt/str]
   (if (fs/exists? path)
     (slurp path)
     (commit! path "{}"))))

(defn default-file-commit-with [{:keys [connection write-with] :as this}]
  (commit! connection (write-with @this)))

(defn default-file-snapshot-with [{:keys [connection read-with]}]
  (read-with (snapshot connection)))

(defmethod baby.pat.supatom/commit-with :file/default [supatom]
  (default-file-commit-with supatom))
(defmethod baby.pat.supatom/snapshot-with :file/default [supatom]
  (default-file-snapshot-with supatom))

(def supatom-file-default-overlay {:variant :file/default
                                   :backing :file
                                   :write-with str
                                   :read-with clojure.edn/read-string})

(defn supatom [config]
  (let [config (if (string? config)
                 (assoc supatom-file-default-overlay :connection config)
                 (merge supatom-file-default-overlay config))
        {:keys [connection]} config
        _ (when-not (fs/exists? connection)
            (fs/create-dirs (str (fs/parent connection)))
            (spit connection "{}"))]
    (baby.pat.supatom/supatom-> config)))

(comment
  (def fff "bin/resources/supatoms/hello.atom")
  (def ggg "bin/resources/supatoms/nice.atom")
  (slurp fff)
  (fs/delete-if-exists fff)
  (fs/delete-if-exists ggg)
  ;(create-file fff)
  (def aaa (supatom fff))
  (def bbb (supatom ggg))
  
  (reset! aaa {:a 9})
  (reset! bbb {:b 10})

  @bbb
  (def commit-ignored! (partial commit! :ignored))
  (def snapshot-ignored! (partial snapshot! :ignored))

  (commit! :file/player-test (id->fs-path :player-test/fun-times) (slurp "scratchpad/heyo.repl"))

;;
  )
