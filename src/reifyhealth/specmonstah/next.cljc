(ns reifyhealth.specmonstah.next
  (:require [loom.graph :as lg]
            [loom.alg :as la]
            [loom.alg-generic :as lag]
            [loom.derived :as ld]
            [medley.core :as medley]
            [clojure.set :as set])
  (:refer-clojure :exclude [doall]))

(defn- ent-references
  "Returns `[ent-type ent-name]` pairs from a ref map"
  [refs]
  (->> refs
       vals
       (map #(vec (take 2 %)))
       set))

(defn- relation-template-graph
  [relation-template]
  (->> relation-template
       (medley/map-vals (comp #(map first %) vals first))
       (reduce-kv (fn [node-pairs ent-type ref-types]
                    (into node-pairs (map (partial vector ent-type) ref-types)))
                  [])
       (apply lg/digraph)))

(defn- digraph-slice
  [graph start end]
  (lg/subgraph graph
               (set/intersection (-> (ld/subgraph-reachable-from graph start)
                                     lg/nodes
                                     set)
                                 (-> (lg/transpose graph)
                                     (ld/subgraph-reachable-from end)
                                     lg/nodes
                                     set))))

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
                                  ;; if the config manually sets a ref value, don't
                                  ;; include the ref'd type in the final config. also,
                                  ;; skip if name is there already
                                  (or (get ent-attrs template-ref-attr)
                                      (and (get expanded-config ref-name)
                                           (not= ref-name template-ref-type)))
                                  expanded-config

                                  ;; refs are provided by relation template
                                  (= ref-name template-ref-type)
                                  (cond-> (update-in expanded-config [ent-name 1] assoc template-ref-attr ref-name)
                                    (not (get expanded-config template-ref-type))
                                    (merge (expand-config-refs {template-ref-type [template-ref-type]} relation-template)))

                                  ;; custom named ref, not defined in config
                                  :else
                                  (merge expanded-config
                                         (expand-config-refs {ref-name [template-ref-type]} relation-template)))))
                            expanded-config
                            ent-template-refs)))
             config
             config))

(defn binding-graph
  "Return a subgraph of all nodes that need to be "
  [relation-template start end]
  (-> (relation-template-graph relation-template)
      (digraph-slice start end)))

(defn expand-config-binding
  [config ent-name ent-type base-name ent-ref-attr ent-ref-name relation-template binding-nodes]
  (let [relation-map      (-> (get-in relation-template [ent-type 0])
                              (set/map-invert))
        relation-type-map (medley/map-keys first relation-map)
        bound-attr        (get relation-map ent-ref-attr)
        ent-config        (cond-> (get config ent-name [ent-type {}])
                            bound-attr (update-in [1 bound-attr] #(or % ent-ref-name))
                            true       (update-in [1] dissoc ent-ref-attr))]

    (reduce (fn [config binding-node]
              (if-let [binding-graph-ref-attr (binding-node relation-type-map)]
                (let [binding-graph-ref-path [ent-name 1 binding-graph-ref-attr]
                      binding-graph-ref-name (get-in config binding-graph-ref-path (keyword (name base-name) (name binding-node)))]
                  (expand-config-binding (assoc-in config binding-graph-ref-path binding-graph-ref-name)
                                         binding-graph-ref-name
                                         binding-node
                                         base-name
                                         ent-ref-attr
                                         ent-ref-name
                                         relation-template
                                         binding-nodes))
                config))
            (assoc config ent-name ent-config)
            binding-nodes)))

(defn expand-config-bindings
  [config relation-template]
  (reduce-kv (fn [expanded-config ent-name [ent-type ent-refs ent-attrs]]
               (reduce-kv (fn [expanded-config ent-ref-attr ent-ref-name]
                            (if (vector? ent-ref-attr)
                              ;; walk all the ents, create
                              (expand-config-binding expanded-config
                                                     ent-name
                                                     ent-type
                                                     ent-name
                                                     ent-ref-attr
                                                     ent-ref-name
                                                     relation-template
                                                     (lg/nodes (binding-graph relation-template ent-type (first ent-ref-attr))))
                              expanded-config))
                          expanded-config
                          ent-refs))
             config
             config))

(defn expand-config
  [config relation-template]
  (->  config
       (expand-config-bindings relation-template)
       (expand-config-refs relation-template)))

(defn gen-db
  [config relation-template]
  (expand-config config relation-template))
