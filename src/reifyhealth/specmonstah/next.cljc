(ns reifyhealth.specmonstah.next
  (:require [loom.graph :as g]
            [loom.alg :as la]
            [medley.core :as medley])
  (:refer-clojure :exclude [doall]))

(defn expand-config-refs
  [config relation-template]
  (reduce-kv (fn [expanded-config ent-name [ent-type ent-refs ent-attrs]]
               (let [[ent-template-refs] (get relation-template ent-type)]
                 ;; add the ref to the expanded template, and add it to the ent refs
                 ;; template-ref-attr is e.g. :book-id
                 ;; template-ref-type is e.g. ::book
                 (reduce-kv (fn [expanded-config template-ref-attr [template-ref-type]]
                              ;;
                              (let [ref-name (get ent-refs template-ref-attr template-ref-type)]
                                (cond
                                  ;; if the config manually sets a ref
                                  ;; value, don't include the ref'd
                                  ;; type in the final config. also,
                                  ;; skip if name is there already
                                  (or (get ent-attrs template-ref-attr)
                                      (and (get expanded-config ref-name)
                                           (not= ref-name template-ref-type)))
                                  expanded-config

                                  ;; refs are provided by relation template
                                  (= ref-name template-ref-type)
                                  (cond-> (update-in expanded-config [ent-name 1] assoc template-ref-attr ref-name)
                                    (not (get expanded-config template-ref-type))
                                    (merge (expand-config {template-ref-type [template-ref-type]} relation-template)))

                                  ;; custom named ref, not defined in config
                                  :else
                                  (merge expanded-config
                                         (expand-config {ref-name [template-ref-type]} relation-template)))))
                            expanded-config
                            ent-template-refs)))
             config
             config))

(defn expand-config-bindings
  [config relation-template]
  config)

(defn expand-config
  [config relation-template]
  (-> (expand-config-refs config relation-template)
      (expand-config-bindings relation-template)))

(defn gen-db
  [config relation-template]
  (expand-config config relation-template))
