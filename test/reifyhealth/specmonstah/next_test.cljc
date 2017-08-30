(ns reifyhealth.specmonstah.next-test
  (:require #?(:clj [clojure.test :refer [deftest is use-fixtures testing]]
               :cljs [cljs.test :include-macros true])
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen :include-macros true]
            [reifyhealth.specmonstah.next :as sm]))

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

(def relation-template
  {::author []
   ::publisher []
   ::book [{:author-id [::author :id]
            :publisher-id [::publisher :id]}]
   ::chapter [{:book-id [::book :id]}]})

{::book [::book]}

(deftest expand-config-refs-atom
  (is (= (#'sm/expand-config-refs {::author [::author]} relation-template)
         {::author [::author]})))

(deftest expand-config-refs
  (is (= (#'sm/expand-config-refs {:b1 [::book]} relation-template)
         {:b1 [::book {:author-id ::author
                       :publisher-id ::publisher}]
          ::author [::author]
          ::publisher [::publisher]})))

(deftest expand-config-refs-deep
  (is (= (#'sm/expand-config-refs {:c1 [::chapter]} relation-template)
         {:c1 [::chapter {:book-id ::book}]
          ::book [::book {:author-id ::author
                          :publisher-id ::publisher}]
          ::author [::author]
          ::publisher [::publisher]})))

(deftest expand-config-refs-multi-refs
  (is (= (#'sm/expand-config-refs {:c1 [::chapter {:book-id :b1}]} relation-template)
         {:c1 [::chapter {:book-id :b1}]
          :b1 [::book {:author-id ::author
                       :publisher-id ::publisher}]
          ::author [::author]
          ::publisher [::publisher]})))



(comment
  (insert-all [{:c1 [::chapter]}
               {:c2 [::chapter]}])

  (insert-all [::chapter ::chapter])

  (insert-all [{:c1 [::chapter {:book-id :b1}]}
               {:c2 [::chapter {:book-id :b2
                                [::author :id] :a1}]}]
              {:b1 [::book]
               :b2 [::book]})

  (insert-all {:b1 [::book]
               :b2 [::book]

               :c2 [::chapter {:book-id :b2
                               [::author :id] :a1}]
               :c1 [::chapter {:book-id :b1} {:chapter-name "blar"}]}))
