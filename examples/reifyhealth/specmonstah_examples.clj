(ns reifyhealth.specmonstah-examples
  (:require [reifyhealth.specmonstah.core :as rs]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]))

(s/def ::id pos-int?)

(s/def ::publisher-name #{"Default Publisher Name"})
(s/def ::publisher (s/keys :req-un [::id ::publisher-name]))

(s/def ::book-name #{"Default Book Name"})
(s/def ::book (s/keys :req-un [::id ::book-name]))

(s/def ::chapter-name #{"Default Chapter Title"})
(s/def ::chapter (s/keys :req-un [::id ::chapter-name]))

(defn gen1
  [spec]
  (gen/generate (s/gen spec)))

(def relations
  (rs/expand-relation-template
    {::publisher [{}]
     ::book [{:publisher-id [::publisher :id]}]
     ::chapter [{:book-id [::book :id]}]}))

(def result-1 (rs/gen-tree gen1 relations [::chapter]))
#_{::publisher {::rs/template {:id 1, :publisher-name "Default Publisher Name"}}
   ::book {::rs/template {:id 192108, :book-name "Default Book Name", :publisher-id 1}}
   ::rs/query [[::rs/chapter {:id 213, :chapter-name "Default Chapter Title", :book-id 192108}]]
   ::rs/order [[::publisher ::rs/template]
               [::book ::rs/template]]}

(get-in result-1 [::publisher ::rs/template])
; => {:id 1, :publisher-name "Default Publisher Name"}

(get-in result-1 [::book ::rs/template])
; => {:id 192108, :book-name "Default Book Name", :publisher-id 1}

(def result-2 (rs/gen-tree gen1 relations [::chapter ::book]))
(::rs/query result-2)
#_[[::chapter {:id 69189760, :chapter-name "Default Chapter Title", :book-id 6119}]
   [::book {:id 6938682, :book-name "Default Book Name", :publisher-id 9365}]]

(def result-3 (rs/gen-tree gen1 relations [::book [::book {:publisher-id :p1}]]))

(def result-4 (rs/gen-tree gen1 relations [::book
                                           [::book {:publisher-id :p1}]
                                           [::book {:publisher-id :p1}]]))

(def result-5 (rs/gen-tree gen1 relations [::chapter
                                           [::chapter {:book-id [:b1 {:publisher-id :p1}]}]]))

(def result-6 (rs/gen-tree gen1 relations [[::chapter {} {:chapter-name "Custom Chapter Name"}]
                                           [::book {:publisher-id [:p1 {} {:publisher-name "Custom Publisher Name"}]}]]))

(def inserted-records (atom []))

(defn insert!
  [record]
  (swap! inserted-records conj record))

(rs/doall insert! gen1 relations
          [[::chapter]
           [::chapter]
           [::chapter {:book-id [:b1 {} {:book-name "Custom Book Name"}]}
                      {:chapter-name "Custom Chapter Name"}]])
@inserted-records
