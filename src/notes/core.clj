(ns notes.core
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as st]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.tools.cli :as cli])
  (:import
   (java.io File
            RandomAccessFile)
   (java.nio MappedByteBuffer)
   (java.nio.channels FileChannel$MapMode)
   (java.time LocalDate
              LocalTime)
   (java.time.format DateTimeFormatter)
   (org.iq80.snappy Snappy))
  (:gen-class))

(s/def ::hour (s/and int? (partial > 24) (complement neg?)))
(s/def ::minute (s/and int? (partial > 60) (complement neg?)))
(s/def ::second (s/and int? (partial > 60) (complement neg?)))
(s/def ::timecode (s/keys :req [::hour ::minute]
                          :opt [::second]))
(s/def ::year number?)
(s/def ::month #{:january :febuary :march
                 :april :may :june
                 :july :august :september
                 :october :november :december})
(s/def ::day number?)
(s/def ::date (s/keys :req [::day ::month ::year]
                      :opt [::timecode]))

(s/def ::topic keyword?)
(s/def ::tag keyword?)
(s/def ::tags (s/coll-of ::tag))
(s/def ::note (s/keys :req [::date ::content]
                      :opt [::title ::topic ::tags]))

(s/def ::location int?)
(s/def ::size pos-int?)
(s/def ::compressed-size pos-int?)
(s/def ::metadata (s/keys :req [::location ::size ::compressed-size ::date]
                          :opt [::title ::topic ::tags]))

(defn num->month
  [^long n]
  (case n
    1 :january
    2 :febuary
    3 :march
    4 :april
    5 :may
    6 :june
    7 :july
    8 :august
    9 :september
    10 :october
    11 :november
    12 :december))
(s/fdef num->month
  :args (s/cat :num (s/and pos-int?
                           (partial > 13))))

(defn timecode
  ([& {:keys [hour minute second]}]
   (let [ret {::hour hour
              ::minute minute}]
     (if second
       (assoc ret ::second second)
       ret))))
(s/fdef timecode
  :args (s/keys* :req-un [::hour ::minute]
                 :opt-un [::second])
  :ret ::timecode)

(defn timecode-from-java
  [^LocalTime local-time]
  (let [hour-of-day (.getHour local-time)
        minute-of-hour (.getMinute local-time)
        second-of-minute (.getSecond local-time)]
    {::hour hour-of-day
     ::minute minute-of-hour
     ::second second-of-minute}))
(s/fdef timecode-from-java
  :args (s/cat :local-time (partial instance? java.time.LocalTime))
  :ret ::timecode)

(defn date
  [& {:keys [month day year timecode]}]
  (let [ret {::month month
             ::day day
             ::year year}]
    (if timecode
      (assoc ret ::timecode timecode)
      ret)))
(s/fdef date
  :args (s/keys* :req-un [::month ::day ::year]
                 :opt-un [::timecode]))

(defn date-from-java
  ([^LocalDate local-date]
   (let [day-of-month (.getDayOfMonth local-date)
         month-of-year (.getMonthValue local-date)
         year (.getYear local-date)]
     {::day day-of-month
      ::month (num->month month-of-year)
      ::year year}))
  ([^LocalDate local-date ^LocalTime local-time]
   (assoc (date-from-java local-date)
          ::timecode (timecode-from-java local-time))))
(s/fdef date-from-java
  :args (s/or :date (s/cat :local-date (partial instance? java.time.LocalDate))
              :date-time (s/cat :local-date (partial instance? java.time.LocalDate)
                                :local-time (partial instance? java.time.LocalTime)))
  :ret ::date)

(defn current-date
  []
  (date-from-java (LocalDate/now) (LocalTime/now)))
(s/fdef current-date
  :args (s/cat)
  :ret ::date)

(defn compare-timecode
  [t1 t2]
  (let [v1 [(::hour t1) (::minute t1) (::second t1)]
        v2 [(::hour t2) (::minute t2) (::second t2)]]
    (compare v1 v2)))
(s/fdef compare-timecode
  :args (s/cat :t1 ::timecode
               :t2 ::timecode)
  :ret number?)

(defn compare-date
  [d1 d2]
  (let [v1 [(::year d1) (::month d1) (::day d1)]
        v2 [(::year d2) (::month d2) (::day d2)]
        res (compare v1 v2)]
    (if (and (zero? res)
             (::timecode d1)
             (::timecode d2))
      (compare-timecode (::timecode d1) (::timecode d2))
      res)))
(s/fdef compare-date
  :args (s/cat :d1 ::date
               :d2 ::date)
  :ret number?)

(defn note
  [& {:keys [date content title topic tags]}]
  (let [ret {::date date
             ::content content}
        ret (or (when title
                  (assoc ret ::title title))
                ret)
        ret (or (when topic
                  (assoc ret ::topic topic))
                ret)
        ret (or (when tags
                  (assoc ret ::tags tags))
                ret)]
    ret))
(s/fdef note
  :args (s/keys* :req-un [::date ::content]
                 :opt-un [::title ::topic ::tags]))

(defn format-timecode
  [{::keys [hour minute second]}]
  (str (if (> hour 12)
         (format "%d:%02d:%02d " (- hour 12) minute second)
         (format "%d:%02d:%02d " (if (== 0 hour)
                                   12
                                   hour) minute second))
       (if (> hour 11)
         "PM"
         "AM")))
(s/fdef format-timecode
  :args (s/cat :timecode ::timecode)
  :ret string?)

(defn format-date
  [{::keys [timecode year month day]}]
  (let [month (name month)
        month (str (.toUpperCase (.substring month 0 1)) (.substring month 1))]
    (str (when timecode
           (str (format-timecode timecode)
                ", "))
         month " " day ", " year)))
(s/fdef format-date
  :args (s/cat :date ::date)
  :ret string?)

(defn note-text
  [{::keys [date content title topic tags]}]
  (->> [(or title
            "No Title")
        (format-date date)
        (if (and topic
                 (seq tags))
          (apply str (name topic) ": " (interpose ", " (map name tags)))
          (do (when topic
                (name topic))
              (when tags
                (interpose ", " (map name tags)))))
        (if (sequential? content)
          (str/join \space content)
          (str content))]
       (filter identity)
       (str/join \newline)))

(def ^{:dynamic true :tag java.io.File} *db-file*
  (File. (System/getProperty "user.home") ".notes.db"))

(defn append-entry
  [note]
  (when-not (.exists *db-file*)
    (spit *db-file* nil))
  (with-open [fc (.getChannel (RandomAccessFile. *db-file* "rw"))]
    (let [meta-str (prn-str (select-keys note [::topic ::title ::tags ::date]))
          meta-arr (Snappy/compress (.getBytes meta-str))
          note-str (prn-str note)
          note-arr (Snappy/compress (.getBytes note-str))
          buffer (.map fc FileChannel$MapMode/READ_WRITE
                       (.size fc)
                       (+ (* 5 Long/BYTES)
                          (count meta-arr)
                          (count note-arr)))]
      (.putLong buffer (+ (* 4 Long/BYTES) (count meta-arr) (count note-arr)))
      (.putLong buffer (count meta-str))
      (.putLong buffer (count meta-arr))
      (.put buffer meta-arr)
      (.putLong buffer (count note-str))
      (.putLong buffer (count note-arr))
      (.put buffer note-arr)))
  nil)
(s/fdef append-entry
  :args (s/cat :note ::note)
  :ret nil?)

(defn- read-metadata
  [^MappedByteBuffer buffer loc]
  (let [size (.getLong buffer)
        meta-len (int (.getLong buffer))
        meta-arr-len (.getLong buffer)
        meta-arr ^bytes (make-array Byte/TYPE meta-arr-len)]
    (.get buffer meta-arr)
    (let [meta-str (String. (Snappy/uncompress meta-arr 0 meta-arr-len))
          tags (assoc (edn/read-string meta-str)
                      ::location (+ loc (* Long/BYTES 5) meta-arr-len)
                      ::size (.getLong buffer)
                      ::compressed-size (.getLong buffer))
          next-location (+ Long/BYTES size loc)]
      [tags next-location])))
(s/fdef read-metadata
  :args (s/cat :buffer (partial instance? java.nio.MappedByteBuffer)
               :loc pos-int?)
  :ret (s/tuple ::metadata
                number?))

(defn entry-metadata
  "Returns a lazy sequence of the entries and their titles, tags, and topics."
  []
  (when-not (.exists *db-file*)
    (spit *db-file* nil))
  (let [fc (.getChannel (RandomAccessFile. *db-file* "r"))
        lazy-fn (fn f [location]
                  (let [memory-mapped-file (.map fc
                                                 FileChannel$MapMode/READ_ONLY
                                                 location
                                                 (min (* 1024 8)
                                                      (- (.size fc)
                                                         location)))]
                    (lazy-seq (if (<= (.limit memory-mapped-file) 0)
                                (do (.close fc)
                                    nil)
                                (let [[tags next-location]
                                      (read-metadata memory-mapped-file location)]
                                  (cons tags (f next-location)))))))]
    (lazy-fn 0)))
(s/fdef entry-tags
  :args (s/cat)
  :ret (s/coll-of ::metadata))

(defn read-entry
  [metadata]
  (when-not (.exists *db-file*)
    (spit *db-file* nil))
  (with-open [fc (.getChannel (RandomAccessFile. *db-file* "r"))]
    (let [^MappedByteBuffer buffer (.map fc
                                         FileChannel$MapMode/READ_ONLY
                                         (::location metadata)
                                         (::compressed-size metadata))
          arr ^bytes (make-array Byte/TYPE (::compressed-size metadata))]
      (.get buffer arr)
      (edn/read-string (String. (Snappy/uncompress arr 0 (int (::compressed-size metadata))))))))
(s/fdef read-entry
  :args (s/cat :metadata ::metadata)
  :ret ::note)

(defn filter-xf
  [& {:keys [title topic tags before after date] :as args}]
  (let [title-terms (when title
                      (map str/lower-case (str/split title #"\s+")))]
    (comp (filter (if topic
                    #(= topic (::topic %))
                    (constantly true)))
          (filter (if before
                    #(>= (compare-date before (::date %)) 0)
                    (constantly true)))
          (filter (if after
                    #(<= (compare-date after (::date %)) 0)
                    (constantly true)))
          (filter (if date
                    #(= (compare-date date (::date %)) 0)
                    (constantly true)))
          (filter (if tags
                    #(empty? (st/difference tags (::tags %)))
                    (constantly true)))
          (filter (if title-terms
                    #(some (partial str/includes? (str/lower-case (::title %)))
                           title-terms)
                    (constantly true))))))

(defn read-xf
  [& {:keys [content]}]
  (let [content-terms (when content
                        (map str/lower-case (str/split content #"\s+")))]
    (comp (map read-entry)
          (filter (if content-terms
                    #(some (partial str/includes? (str/lower-case (::content %)))
                           content-terms)
                    (constantly true)))
          (map note-text))))

(def succinct-xf (comp (filter #(some % [::topic ::title ::tags]))
                       (map note-text)))

(defn usage
  [summary]
  (->> ["This is a simple notes application which uses a strange database backend."
        ""
        "Usage: notes [options] action"
        ""
        "Options:"
        summary
        ""
        "Actions:"
        "  add <message>      Adds the message to your personal database"
        "  search <keywords>  Searches for all notes with the keywords"]
       (str/join \newline)))

(def cli-options
  ""
  [[nil "--help" "Prints this message"]
   ["-T" "--topic TOPIC" "Sets the topic on an add, and required the topic on a search"
    :parse-fn keyword]
   [nil "--title \"TITLE\"" "Sets the title on an add, and keyword searches the title on search"]
   ["-t" "--tags \"[TAG*]\"" "Sets the tags on an add, and requires tags on a search"
    :parse-fn #(let [s (edn/read-string %)]
                 (set (map keyword s)))]
   ["-f" "--file FILE" "Specifies a file to be used, useful when you have multiples (default ~/.notes.db)"]
   ["-n" "--num-results NUM-RESULTS" "Specifies how many results should be given in a search, either a number, or \"all\" (default 1)"
    :parse-fn #(if (= % "all")
                 %
                 (Long/parseLong %))]
   ["-r" "--reverse" "Include this flag in a search if you want to reverse the order of results"]
   ["-a" "--after MM-DD-YYYY" "Search only for notes after this date"
    :parse-fn #(let [date-format (DateTimeFormatter/ofPattern "MM-dd-uuuu")]
                 (date-from-java (LocalDate/parse % date-format) (LocalTime/of 0 0)))]
   ["-b" "--before MM-DD-YYYY" "Search only for notes before this date"
    :parse-fn #(let [date-format (DateTimeFormatter/ofPattern "MM-dd-uuuu")]
                 (date-from-java (LocalDate/parse % date-format) (LocalTime/of 23 59)))]
   ["-d" "--date MM-DD-YYYY" "Add the note to this date, or search for a note on this exact date"
    :parse-fn #(let [date-format (DateTimeFormatter/ofPattern "MM-dd-uuuu")]
                 (date-from-java (LocalDate/parse % date-format)))]
   ["-s" "--succinct" "Searching only provides the metadata about the item when this flag is set"]])

(defn apply-keyword-args
  [f & args]
  (apply f (concat (butlast args) (into [] cat (last args)))))

(defn -main
  [& args]
  (let [{:keys [options arguments summary errors] :as parsed-args}
        (cli/parse-opts args cli-options)]
    (if (:help options)
      (when summary
        (println (usage summary)))
      (if errors
        (println "Invalid command. Try \"notes --help\" to see how the program is used")
        (binding [*db-file* (or (when (:file options)
                                  (File. ^String (:file options)))
                                *db-file*)]
          (if (seq arguments)
            (case (str/lower-case (first arguments))
              "add" (do (println "Adding a new ")
                        (append-entry (note :content (rest arguments)
                                            :title (:title options)
                                            :topic (:topic options)
                                            :tags (:tags options)
                                            :date (or (:date options)
                                                      (current-date)))))
              "search" (let [xf (apply-keyword-args
                                 filter-xf
                                 options)
                             xf (if-not (:succinct options)
                                  (comp xf
                                        (read-xf :content (str/join \space (rest arguments))))
                                  (comp xf
                                        succinct-xf))
                             entries (sequence xf (entry-metadata))
                             entries (if (:reverse options)
                                       (reverse entries)
                                       entries)
                             entries (if (= (:num-results options) "all")
                                       entries
                                       (take (or (:num-results options)
                                                 1)
                                             entries))]
                         (println (str "Searching for "
                                       (or (:num-results options) 1)
                                       " entries in your notes...\n"))
                         (doseq [entry (interpose "\n" entries)]
                           (println entry)))
              (println "Invalid command. Try \"notes --help\" to see how the program is used"))
            (println "Invalid command. Try \"notes --help\" to see how the program is used")))))))
