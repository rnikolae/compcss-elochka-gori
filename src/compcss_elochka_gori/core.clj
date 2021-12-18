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
            (> (count (str current))
               (count (str prev)))
            (recur (next nodes)
                   current
                   node
                   (str result "\n" current)
                   index)
            (> (count current) index)
            (recur (next nodes)
                   node
                   node
                   result
                   (if (< (count (str current)) max-w)
                     (count (str current))
                     max-w))
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
      (- (Math/ceil (/ (count (str vetka)) 2)))
      (repeat " ")
      (clojure.string/join)
      (str vetka)))

(defn elochka-centering [elochka]
  (->> (clojure.string/split elochka #"\n")
       (map (partial center-computing (-> (sort-by (comp count str) elochka) last count)))
       (clojure.string/join "\n")))

(defn elochka-gori [css]
  (-> (css-blocks css)
      (elochka-create)
      (elochka-centering)))

(defn middleware
  [configuration db]
  (->>
   (:compcss.core/output-stylesheets db)
   (clj-ph-css.core/schema->string)
   (elochka-gori)
   (spit (get-in configuration [:output :css]))))

