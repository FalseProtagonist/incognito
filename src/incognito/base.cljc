(ns incognito.base
  (:require [clojure.string :as str]))

(defrecord IncognitoTaggedLiteral [tag value])

(defn incognito-reader [read-handlers m]
  (if (read-handlers (:tag m))
    ((read-handlers (:tag m)) (:value m))
    (map->IncognitoTaggedLiteral m)))

(defn normalize-ns [s]
  (-> s
      (str/replace-first "/" ".")
      (str/replace "-" "_")))

;; lifted from fress class-sym in fress/writer
#?(:cljs 
   (defn class-sym
     "Record types need a string so the name can survive munging. Is converted to
      symbol before serializing."
     [rec rec->tag]
     (let [name (get rec->tag (type rec))]
       (if (string? name)
         (symbol name)
         (throw (js/Error. "writing records requires corresponding entry in *record->name*"))))))

(def debug-atom (atom {:lookup-records true}))


(defn incognito-writer [write-handlers r & [rec->tag]]
  (let [s                   #?(:cljs
                               (do
                                 (if rec->tag
                                   (if (:lookup-records @debug-atom)
                                     (class-sym r rec->tag)
                                    ;; pretty sure this is here from legacy debugging
                                    ;; and this branch can be removed
                                     (-> r type pr-str normalize-ns symbol))
                                  (-> r type pr-str normalize-ns symbol)
                                   #_(do
                                     (println "changed error")
                                    ;;  (println (.trace js/console))
                                     (let [er (js/Error. "need rec->tag for clojurescript or names won't survive munging, see fress docs")]
                                       (println "stack " (.-stack er))
                                       (throw er))
                                     )))
                               :clj
                               (-> r type pr-str normalize-ns symbol))
        break-map-recursion (if (map? r) (into {} r) r)
        [tag v]             (if (write-handlers s)
                              [s ((write-handlers s) break-map-recursion)]
                              [s break-map-recursion]
                              #_(pr-str->pure-read-string r))]
    {:tag   tag
     :value v}))

