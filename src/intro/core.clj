(ns intro.core
  (:gen-class)
  (:require [clojure.pprint :refer [cl-format]]))

(defonce db (atom []))

(defn make-cd [title artist rating ripped]
  {:title title :artist artist :rating rating :ripped ripped})

(defn add-record [cd]
  (swap! db conj cd))

(defn dump-db []
  (doseq [cd @db]
    (cl-format true "~{~a:~10t~a~%~}~%" (flatten (into [] cd)))))

(defn prompt-read [prompt]
  (println (str prompt ": "))
  (read-line))

(defn y-or-n? [prompt]
  (println prompt)
  (loop [answer (read-line)]
    (if (re-matches #"^y|n$" answer)
      (condp = answer
        "y" true
        "n" false)
      (recur (read-line)))))

(defn prompt-for-cd []
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (try (Integer/parseInt (prompt-read "Rating")) (catch Exception e 0))
    (y-or-n? "Ripped [y/n]: ")))

(defn add-cds []
  (loop [record (prompt-for-cd)]
    (add-record record)
    (if (not (y-or-n? "Another? [y/n]: "))
      nil
      (recur (prompt-for-cd)))))

(defn save-db [filename]
  (spit filename @db))

(defn load-db [filename]
  (reset! db (load-file filename)))

(defn select-by-artist [artist]
  (filterv #(= artist (:artist %)) @db))

(defn select [fn]
  (filterv fn @db))

(defn artist-selector [artist]
  #(= artist (:artist %)))

(defn where [& {:keys [title artist rating ripped]
                :as attrs}]
  (fn [cd]
    (and
      (if title (= (:title cd) title) true)
      (if artist (= (:artist cd) artist) true)
      (if rating (= (:rating cd) rating) true)
      (if ripped (= (:ripped cd) ripped) true))))

(defn update-row [row {:keys [title artist rating ripped]}]
  (assoc row
         :title (if (nil? title) (:title row) title)
         :artist (if (nil? artist) (:artist row) artist)
         :rating (if (nil? rating) (:rating row) rating)
         :ripped (if (nil? ripped) (:ripped row) ripped)))

(defn update [selector-fn & {:keys [title artist rating ripped] :as attrs}]
  (swap! db #(map (fn [row]
                      (if (selector-fn row)
                        (update-row row attrs)
                        row))
                  %)))

(defn delete [selector-fn]
  (swap! db (fn [db] (remove #(selector-fn %) db))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
