(ns compcss-elochka-gori.core
  (:require [clojure.string]
            [clojure.java.io]))

(defn elochka-create
  [blocks]
  (let [max-w 300]
    (loop [nodes   (seq blocks)
           prev    ""
           current (first blocks)
           result  (first blocks)
           index   15]
      (if (seq nodes)
        (let [node (first nodes)]
          (cond
            (> (count current)
               (count prev))
            (recur (next nodes)
                   current
                   node
                   (str result "\n" current)
                   index)
            (> (count current) index)
            (do (prn (count current))
                (recur (next nodes)
                       node
                       node
                       result
                       (if (< (count current) max-w)
                         (count current)
                         max-w)))
            :else
            (recur (next nodes)
                   prev
                   (str current node)
                   result
                   index)))
        result))))

(defn css-blocks [css]
  (-> css
      (clojure.string/replace
       #";|,|\{|}"
       {";" ";\n"
        "," ",\n"
        "{" "{\n"
        "}" "}\n"})
      (clojure.string/split #"\n")))

(defn center-computing [max-size vetka]
  (-> max-size
      (- (Math/ceil (/ (count vetka) 2)))
      (repeat " ")
      (clojure.string/join)
      (str vetka)))

(defn elochka-centering [elochka]
  (->> (clojure.string/split elochka #"\n")
       (map (partial center-computing (-> (sort-by count elochka) last count)))
       (clojure.string/join "\n")))

(defn elochka-gori [css]
  (-> (css-blocks css)
      (elochka-create)
      (elochka-centering)))

(defn middleware
  [condiguration db]
  (-> (slurp "some-file ") 
      (elochka-gori) 
      (spit "some-file")))

