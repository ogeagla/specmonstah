(ns reifyhealth.specmonstah.next-test
  (:require #?(:clj [clojure.test :refer [deftest is are use-fixtures testing]]
               :cljs [cljs.test :include-macros true])
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen :include-macros true]
            [reifyhealth.specmonstah.next :as sm]
            [loom.graph :as lg]
            [loom.alg :as la]))

(def id-seq (atom 0))

(s/def ::id
  (s/with-gen
    pos-int?
    #(gen/fmap (fn [_] (swap! id-seq inc)) (gen/return nil))))

(s/def ::author-name #{"Fabrizio S."})
(s/def ::author (s/keys :req-un [::id ::author-name]))

(s/def ::book-name #{"The Book"})
(s/def ::book (s/keys :req-un [::id ::book-name]))

(s/def ::chapter-name #{"Chapter 1"})
(s/def ::chapter (s/keys :req-un [::id ::chapter-name]))

(s/def ::publisher-name #{"PublishCo"})
(s/def ::publisher (s/keys :req-un [::id ::publisher-name]))

;; ---------
;; Test refs
;; ---------

(def relation-template
  {::author    []
   ::publisher []
   ::book      [{:author-id    [::author :id]
                 :publisher-id [::publisher :id]}]
   ::chapter   [{:book-id [::book :id]}]})

(deftest expand-config-refs-atom
  (is (= (#'sm/expand-config-refs {::author [::author]} relation-template)
         {::author [::author]})))

(deftest expand-config-refs
  (is (= (#'sm/expand-config-refs {:b1 [::book]} relation-template)
         {:b1         [::book {:author-id    ::author
                               :publisher-id ::publisher}]
          ::author    [::author]
          ::publisher [::publisher]})))

(deftest expand-config-refs-deep
  (is (= (#'sm/expand-config-refs {:c1 [::chapter]} relation-template)
         {:c1         [::chapter {:book-id ::book}]
          ::book      [::book {:author-id    ::author
                               :publisher-id ::publisher}]
          ::author    [::author]
          ::publisher [::publisher]})))

(deftest expand-config-refs-multi-refs
  (is (= (#'sm/expand-config-refs {:c1 [::chapter {:book-id :b1}]} relation-template)
         {:c1         [::chapter {:book-id :b1}]
          :b1         [::book {:author-id    ::author
                               :publisher-id ::publisher}]
          ::author    [::author]
          ::publisher [::publisher]})))

(deftest expand-config-refs-deep-multi-refs
  (is (= (#'sm/expand-config-refs {:b1 [::book {:author-id :a1}]
                                   :c1 [::chapter {:book-id :b1}]} relation-template)
         {:c1         [::chapter {:book-id :b1}]
          :b1         [::book {:author-id    :a1
                               :publisher-id ::publisher}]
          :a1         [::author]
          ::publisher [::publisher]})))

;; ---------
;; helpers
;; ---------
(def helper-relation-template
  {:toy         [{:manufacturer-id [:manufacturer :id]}]
   :binkie      [{:manufacturer-id [:manufacturer :id]}]
   :child       [{:toy-id    [:toy :id]
                  :binkie-id [:binkie :id]}]
   :reservation [{:child-id [:child :id]}]})

(def rt-graph (#'sm/relation-template-graph helper-relation-template))

(deftest relation-template-graph
  (is (la/eql? rt-graph
               (lg/digraph [:toy :manufacturer]
                           [:binkie :manufacturer]
                           [:child :toy]
                           [:child :binkie]
                           [:reservation :child]))))

(deftest digraph-slice-keep-all
  (is (la/eql? (#'sm/digraph-slice rt-graph :reservation :manufacturer)
               (lg/digraph [:toy :manufacturer]
                           [:binkie :manufacturer]
                           [:child :toy]
                           [:child :binkie]
                           [:reservation :child]))))

(deftest digraph-slice
  (is (la/eql? (#'sm/digraph-slice rt-graph :reservation :binkie)
               (lg/digraph [:child :binkie]
                           [:reservation :child]))))

;; ---------
;; bindings
;;
;; Bindings assert that the same record should be used throughout an
;; entire graph of ents
;; ---------


;; Modeling a... babysitting service where you make reservations for a
;; children, and each child is allowed one binkie and one toy. And for
;; some reason you care about binkie/toy manufacturer
(s/def ::manufacturer (s/keys :req-un [::id]))

(s/def ::manufacturer-id ::id)
(s/def ::toy (s/keys :req-un [::id ::manufacturer-id]))
(s/def ::binkie (s/keys :req-un [::id ::manufacturer-id]))

(s/def ::toy-id ::id)
(s/def ::binkie-id ::id)
(s/def ::child (s/keys :req-un [::id ::toy-id ::binkie-id]))

(s/def ::child-id ::id)
(s/def ::reservation (s/keys :req-un [::id ::child-id]))

(def binding-relation-template
  {::toy         [{:manufacturer-id [::manufacturer :id]}]
   ::binkie      [{:manufacturer-id [::manufacturer :id]}]
   ::child       [{:toy-id    [::toy :id]
                   :binkie-id [::binkie :id]}]
   ::reservation [{:child-id [::child :id]}]})

(deftest expand-config-bindings
  (is (= (#'sm/expand-config-bindings {:r1 [::reservation {[::manufacturer :id] :m1}]} binding-relation-template)
         {:r1        [::reservation {:child-id :r1/child}]
          :r1/child  [::child {:toy-id    :r1/toy
                               :binkie-id :r1/binkie}]
          :r1/toy    [::toy {:manufacturer-id :m1}]
          :r1/binkie [::binkie {:manufacturer-id :m1}]
          :m1        [::manufacturer {q}]})))

#_(deftest expand-config-bindings-nested
  (is (= (#'sm/expand-config-bindings {:p1 {[::manufacturer :id] :m1}} relation-template)
         {:p1        [::parent {:child-id :p1-child}]
          :p1-child  [::child {:toy-id    :p1-toy
                               :binkie-id :p1-binkie}]
          :p1-toy    [::toy {:manufacturer-id :m1}]
          :p1-binkie [::binkie {:manufacturer-id :m1}]})))

(comment
  (insert-all [{:c1 [::chapter]}
               {:c2 [::chapter]}])

  (insert-all [::chapter ::chapter])

  (insert-all [{:c1 [::chapter {:book-id :b1}]}
               {:c2 [::chapter {:book-id       :b2
                                [::author :id] :a1}]}]
              {:b1 [::book]
               :b2 [::book]})

  (insert-all {:b1 [::book]
               :b2 [::book]

               :c2 [::chapter {:book-id       :b2
                               [::author :id] :a1}]
               :c1 [::chapter {:book-id :b1} {:chapter-name "blar"}]}))
