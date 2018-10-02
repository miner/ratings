(ns miner.ratings
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(def rats (delay (html/html-resource (io/as-file "resources/USAPA-ratings-Oct1.html"))))


(def cols [:row :last :first :middle :mw :doubles :mixed :singles
           :legacy-doubles :legacy-singles :city :state :contact])

;; The first :tr is the header.  The rest are players.
;;
;; :row is just a line count, not useful
;; :contact is a weird link to internal email system so it's no good to us

(defn as-rating [x]
  (let [rating (if (number? x)
                 (double x)
                 (if (str/blank? x)
                   0.0
                   (try 
                     (Double/parseDouble x)
                     (catch NumberFormatException _ex
                       (println "Rating:" x "replaced with -1.0")
                       -1.0))))]
    (when-not (zero? rating)
      rating)))

(defn parse-row [row]
  (let [raw (zipmap [:last :first :middle :mw :doubles :mixed :singles
                     :legacy-doubles :legacy-singles :city :state]
                    (drop 1 (butlast (map html/text (html/select row [:td])))))]
    (-> raw
        (dissoc :mw)
        (dissoc :middle)
        (update :doubles as-rating)
        (update :mixed as-rating)
        (update :singles as-rating)
        (update :legacy-doubles as-rating)
        (update :legacy-singles as-rating)
        (assoc :sex (if (= (:mw raw) "F") :female :male)))))


(defn parse-all [rats]
  (let [rows (html/select rats [:table#the_data :tr])]
    ;; skipping header row
    (map parse-row (rest rows))))

(defn load-players []
  (parse-all @rats))



(defn count-at [players key rating]
  (count (filter (fn [p] (= rating (get p key))) players)))

(defn distribution [players key]
  (reduce (fn [res rat]
            (assoc res rat (count-at players key rat)))
          {}
          [6.0 5.5 5.0 4.5 4.0 3.5 3.0]))

;; USAPA says membership over 22,000 (in 2017)
;; I have ratings for about 17,000


;; overall distribution
;; {6.0 21, 5.5 134, 5.0 692, 4.5 2340, 4.0 3975, 3.5 6053}

;; female distribution
;; {6.0 8, 5.5 41, 5.0 229, 4.5 773, 4.0 1474, 3.5 2496}

;; male distribution
;; {6.0 13, 5.5 93, 5.0 463, 4.5 1567, 4.0 2501, 3.5 3557}
