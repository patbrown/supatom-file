(ns baby.pat.supatom.file
  (:require [babashka.fs :as fs]
            [baby.pat.vt :as vt]
            [baby.pat.supatom]
            [clojure.edn]
            [medley.core]
            [orchestra.core :refer [defn-spec]]))

(def !new-paths-require-approval (atom false))

(defn variant->path [variant]
  (str "resources/" (namespace variant) "/" (name variant) "/"))

(defn variant->variant-config [variant]
  {:supatom-variant/id variant
   :supatom-variant/backing :file
   :supatom-variant/connection-variant :file/default
   :supatom-variant/path (variant->path variant)})

(def known-variants #{:file/default :file/ignored :local/babashka :local/build :local/dev :local/main :local/shadow :local/test})

(def supatom-variants-raw
  (mapv variant->variant-config known-variants))

(def supatom-variants (vt/add :default {} supatom-variants-raw))

(defn-spec approved-path ::vt/str
  "Takes a path-prefix which is either a string or a keyword that maps to a known location.
It's simply a layer of indirection that makes for less bullshit if I use the keyword prefixes.   
If for instance I want to change how any of this is done, the only changes need to be in this function.
It's also easily converted into a different dispatch without breaking everything.   "
  ([id ::vt/qkw-or-str] (approved-path supatom-variants :file/default id))
  ([variant ::vt/qkw id ::vt/qkw-or-str] (approved-path supatom-variants variant id))
  ([universe ::vt/map variant ::vt/qkw id ::vt/qkw-or-str]
   (str (vt/<- universe [:supatom-variant/id variant :supatom-variant/path])
        (if (qualified-keyword? id)
          (str (namespace id) "_" (name id))
          id))))

(defn unapproved-path
  ([id] (unapproved-path nil :file/default id))
  ([variant id] (unapproved-path nil variant id))
  ([_ variant id]
   (str (variant->path variant)
        (if (qualified-keyword? id)
          (str (namespace id) "_" (name id))
          id))))

(defn handle-path-approval [universe variant id]
  (if @!new-paths-require-approval
    (approved-path universe variant id)
    (unapproved-path universe variant id)))

(defn-spec create-file ::vt/discard
  "Can be called directly like all others, but generally avoided in favor of using higher level functions."
  ([id ::vt/str] (create-file supatom-variants :file/default id))
  ([variant ::vt/kw-or-str id ::vt/str] (create-file supatom-variants variant id))
  ([universe ::vt/map variant ::vt/kw-or-str id ::vt/str]
   (let [path (handle-path-approval universe variant id)
         parent (fs/parent path)
         _ (when (fs/exists? parent)
             (fs/create-dirs parent))]
     (when-not (fs/exists? path)
       (fs/create-file path)))))

;; # COMMIT!/SNAPSHOT
;; These functions are dumb, but enough.
;
(defn-spec commit! ::vt/discard
  "Commits contents to a file."
  ([id ::vt/qkw contents ::vt/str] (commit! supatom-variants :file/default id contents))
  ([variant ::vt/qkw id ::vt/qkw contents ::vt/str] (commit! supatom-variants :file/default id contents))
  ([universe ::vt/map variant ::vt/qkw id ::vt/qkw contents ::vt/str]
   (let [path (handle-path-approval universe variant id)
         _ (create-file universe variant id)]
     (spit path contents))))

(defn-spec snapshot ::vt/discard
  "Returns a snapshot from a file."
  ([id ::vt/qkw] (snapshot supatom-variants :file/default id))
  ([variant ::vt/qkw id ::vt/qkw] (snapshot supatom-variants variant id))
  ([universe ::vt/map variant ::vt/qkw id ::vt/qkw]
   (let [path (handle-path-approval universe variant id)]
     (if (fs/exists? path)
       (slurp path)
       (commit! universe variant id nil)))))

;; # THE POWER
;; Anything can go here. You can multi dispatch or write new baby.pat.supatom/x-with
;; It's as simple as two functions and a map to dramatically change behavior.
;; Everything in supatom is on top of these. Pretty cool.
(defn default-file-commit-with [{:keys [id connection write-with] :as this}]
  (commit! supatom-variants (:variant connection) id (write-with @this)))
(defn default-file-snapshot-with [{:keys [id connection read-with] :as this}]
  (read-with (snapshot supatom-variants (:variant connection) id)))

(defmethod baby.pat.supatom/commit-with :file/default [supatom]
  (default-file-commit-with supatom))
(defmethod baby.pat.supatom/snapshot-with :file/default [supatom]
  (default-file-snapshot-with supatom))

(defmethod baby.pat.supatom/commit-with :local/babashka [supatom]
  (default-file-commit-with supatom))
(defmethod baby.pat.supatom/snapshot-with :local/babashka [supatom]
  (default-file-snapshot-with supatom))

;; Notice how the read-with/write-with can be anything or nothing.
;; This layer of indirection allows for fun map mixing.   
(def supatom-file-default-overlay {:connection {:variant :file/default}
                                   :variant :file/default
                                   :backing :file
                                   :write-with str
                                   :read-with clojure.edn/read-string})

;; # How to call it.   
;; You call (supatom config-map).   
;; The defaults are supplied.   
;; An empty map works.   
;; Pick and choose what to clobber.
(defn supatom [config]
  (let [config (merge supatom-file-default-overlay config)]
    (baby.pat.supatom/supatom-> config)))

(defn-spec checkout ::vt/supatom [stage ::vt/kw-or-str dt ::vt/kw-or-str]
  (supatom {:id (keyword (vt/singular dt) "id")
            :variant :dt/directory
            :connection {:variant (keyword "local" (name stage))}}))

(comment
  
  (def eee (supatom {:id :night/stick
                     :variant :noway/babashka}))
  
  @eee
  
  (swap! eee assoc :a 9)
  (def fff=eee (supatom {:id :night/stick
                         :variant :local/babashka}))
  @fff=eee
  (reset! ddd {})
  (reset! ccc {})

  (reset! aaa {})
  (type aaa)
  (reset! aaa {})
  (swap! aaa assoc :a 9)
  (def bbb (supatom {:id :too/cool}))
  (def bbb (supatom {:id :what/cool}))

  @aaa
  @bbb

  (swap! bbb assoc :aaa 999)
  @bbb
  (def commit-ignored! (partial commit! :ignored))
  (def snapshot-ignored! (partial snapshot! :ignored))

  (commit! :file/player-test (id->fs-path :player-test/fun-times) (slurp "scratchpad/heyo.repl"))

;;
  )
