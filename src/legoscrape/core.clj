(ns legoscrape.core
  (:import java.net.URL)
  (:import java.util.regex.Pattern)
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [net.cgrand.enlive-html :as en]))

;; Example URL:
;; http://shop.lego.com/en-US/My-First-LEGO-Princess-10656

(comment
  ;; Run the queue of URLs
  (let [rows (pmap extract-url (-> "lego-queue.txt" slurp (string/split #"\s+")))]
    (time
     (clojure.pprint/pprint
      (select-keys
       (compute-group-averages (group-by-categories rows :Pieces :PricePerPiece :Avg-Age :Min-Age))
       ["Friends" "Legends of Chima" "Galaxy Squad" "Ninjago" "Star Wars™" "Buildings"])))))


(def fetch-html (memoize (fn [u] (-> u URL. en/html-resource))))

(defn- logo-url
  [id]
  (format "http://cache.lego.com/e/dynamic/is/image/LEGO/%s" id))

(defn- to-double [s] (Double/parseDouble (last (re-find #"\$?([\d\.]+)" s))))
(defn- to-int    [s] (Integer/parseInt (last (re-find #"\$?([\d]+)" s))))

(defn- attribute-filter
  [for-text]
  (let [re (Pattern/compile (str for-text ":(.+)"))]
   (fn [t]
     (->> t
          (mapcat #(rest (re-find re %1)))
          (filter not-empty)))))

(def attributes
  {:Item       {:sel [:#product-info :.item :em]}
   :Categories {:sel [:#tags :li :a] :trans identity}
   :Age-Range  {:sel [:#product-info :li] :trans (comp first (attribute-filter "Ages"))}
   :Pieces     {:sel [:#product-info :li] :trans (comp to-int
                                                       first
                                                       (attribute-filter "Pieces"))}
   :Price      {:sel [:.product-price :em] :trans (comp int
                                                        (partial * 100)
                                                        to-double
                                                        first)}})

(defn- extract-attribute
  [html conf]
  (let [sel (get conf :sel)
        sep (get conf :sep " ")
        trans (get conf :trans (partial string/join sep))]
    (->> (en/select html sel)
         (map (comp string/trim en/text))
         trans)))

(defn extract-details
  [page]
  (let [html (en/select page [:#product-details])]
    (->> attributes
         (pmap #(vector (first %1) (extract-attribute html (fnext %1))))
         (into {}))))

(defn extract-url
  [url]
  (let [details (-> url fetch-html extract-details)]
   (merge {:URL url
           :Logo (logo-url (:Item details))
           :PricePerPiece (/ (:Price details) (:Pieces details))
           :Avg-Age (let [a (:Age-Range details)]
                      (if (re-find #"[\d]+\+" a)
                        (to-int a)
                        (/ (reduce + (sort (map to-int (string/split a #"\-")))) 2)))
           :Min-Age (to-int (last (re-find #"([\d]+)[\-\+]" (:Age-Range details))))}
          details)))

(defn group-by-categories
  ([rows] (group-by-categories rows nil))
  ([rows & selection]
   (reduce (partial merge-with set/union)
           (map #(into {} (for [c (:Categories %1)] [c #{(if selection (select-keys %1 selection) %1)}]))
                rows))))

(defn compute-group-averages [groups]
  (into {}
   (pmap (fn [group]
          (let [[category rows] group
                size (count rows)]
            [category
             (reduce
              (partial merge-with +)
              {:Size size}
              (map (comp
                    (partial into {})
                    #(map (fn [[k v]] [k (double (/ v size))]) %1)
                    seq)
                   rows))]))
        groups)))