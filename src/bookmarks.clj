(ns bookmarks
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:use [clojure.set :only [union]])
  (:import java.text.SimpleDateFormat
           java.util.Date)
  (:gen-class))

(def usage-base "Usage: java -jar bookmarks-latest.jar BOOKMARKS ")
(def usage-add (str usage-base "add LINK [-t TITLE] [TAGS]"))
(def usage-gen (str usage-base "gen TEMPLATE OUTPUT"))

(def date-serialize "yyyy/MM/dd HH:mm")

(defn tags-from-bookmarks [bookmarks]
  (conj (->> bookmarks
             (map #(get % :tags))
             (apply union)
             (sort))
        :untagged))

;;; Assume the bookmarks are already sorted in reverse chronological order
(defn months-from-bookmarks [bookmarks]
  (let [pretty (SimpleDateFormat. "MMMMMMMMM yyyy")
        serial (SimpleDateFormat. date-serialize)]
    (map #(conj %1 (. pretty format (second %1)))
         (partition 2 1
                    (conj (->> bookmarks
                               (map #(get % :date))
                               (map #(. serial parse %))
                               (map #(. pretty parse (. pretty format %)))
                               (distinct))
                          (. pretty parse "December 3000"))))))

(defn augment-untagged [link] (if (empty? (:tags link))
                                (assoc link :tags #{:untagged}) link))

(defn individual-bookmark-tags [template]
  (html/snippet
   template
   [:#timewise :> html/first-child :> (html/nth-child 2) :> html/first-child
    :ul :> html/first-child]
   [tag]
   [:li :a] (html/do-> (html/content tag)
                       (html/set-attr :href (str "#tag-" tag)))))

(defn individual-bookmark [template ind-tags-snippet]
  (html/snippet
   template
   [:#timewise :> html/first-child :> (html/nth-child 2) :> html/first-child]
   [{:keys [title link date tags]}]
   [:div.desc :a] (html/do-> (html/content title)
                             (html/set-attr :href link))
   [:span.date]   (html/do-> (html/content
                              (if date
                                (. (SimpleDateFormat. "yyyy/MM/dd") format
                                   (. (SimpleDateFormat. date-serialize)
                                      parse date)))))
   [:span.url :a] (html/do-> (html/content link)
                             (html/set-attr :href link))
   [:ul] (html/content (->> tags
                            (map name)
                            (map ind-tags-snippet)))))

(defn tag-toc [template bookmarks]
  (html/snippet
   template
   [:#tagtoc :ul :> html/first-child]
   [tag]
   [:li :span] (html/do->
                (html/content 
                 (str (->> bookmarks
                           (map augment-untagged)
                           (filter #(get (:tags %) tag))
                           (count))))
                (html/set-attr :id (str "count-" (name tag))))
   [:li :a] (let [s (name tag)]
              (html/do-> (html/content s)
                         (html/set-attr :href (str "#tag-" s))))))

(defn timewise [template bookmarks individual-bookmark-snippet]
  (html/snippet
   template
   [:#timewise :> html/first-child]
   [month]
   [:h2] (html/do-> (html/content (first month))
                    (html/set-attr :id (str "date-" (string/replace
                                                     (first month) " " ""))))
   [:ul] (html/content (let [serial (SimpleDateFormat. date-serialize)
                             belongs? (fn [bm]
                                        (let [date (. serial parse (get bm :date))]
                                          (and
                                           (= -1 (compare date (second month)))
                                           (<= 0 (compare date (nth month 2))))))]
                         (->> bookmarks
                              (filter belongs?)
                              (map individual-bookmark-snippet))))))

(defn tagwise [template bookmarks individual-bookmark-snippet]
  (html/snippet
   template
   [:#tagwise :> html/first-child]
   [tag]
   [:h2] (let [s (name tag)]
           (html/do-> (html/content s)
                      (html/set-attr :id (str "tag-" s))))
   [:ul] (html/content (->> bookmarks
                            (map augment-untagged)
                            (filter #(get (get % :tags) tag))
                            (map (fn [link] (assoc link :tags
                                                   (disj (:tags link) tag))))
                            (map individual-bookmark-snippet)))))

(defn entire-page [template]
  (let [ind-bm-snippet (individual-bookmark
                        template (individual-bookmark-tags template))]
    (html/template
     template [bookmarks months tags date]
     [:#datestamp]  (html/content date)
     [:#timewise]   (html/content
                     (map (timewise template bookmarks ind-bm-snippet) months))
     [:#tagtoc :ul] (html/content (map (tag-toc template bookmarks) tags))
     [:#tagwise]    (html/content
                     (map (tagwise template bookmarks ind-bm-snippet) tags)))))

(defn bookmarks-generate
  ([file [template output & rest]]
     (let [bookmarks (->> (read-string (slurp file))
                          (sort-by #(. (SimpleDateFormat. date-serialize)
                                       parse (get % :date)))
                          (reverse))]
       (if-not (and template output)
         (println usage-gen)
         (->> [bookmarks
               (months-from-bookmarks bookmarks)
               (tags-from-bookmarks bookmarks)
               (. (SimpleDateFormat. "yyyy/MM/dd") format (Date.))]
              (apply (entire-page (java.io.File. template)))
              (apply str)
              (println-str)
              (spit output))))))


(defn title-from-web [link]
  (try
    (->> (html/select (html/html-resource (java.net.URL. link))
                      #{[:head :title] [:h1]})
         (map html/text)
         (first))
    ;; MalformedURL, UnknownHost, etc.
    (catch Exception ex link)))

(defn bookmarks-add
  ([file [link title-flag title-opt & rest]]
     (if-not link
       (println usage-add)
       (let [bookmarks (read-string (slurp file))
             [title tags] (if (= title-flag "-t")
                            [title-opt rest]
                            [(title-from-web link)
                             (remove nil? (conj rest title-flag title-opt))])]
         (spit file (prn-str (conj bookmarks
                                   {:title title, :link link,
                                    :date (. (SimpleDateFormat. date-serialize)
                                             format (java.util.Date.))
                                    :tags (set (map keyword tags))})))))))


(defn -main [& args]
  (let [[bookmark-file operation & rest] args]
    (if-not bookmark-file
      (println (str usage-add "\n" usage-gen))
      (case operation
            "add" (bookmarks-add bookmark-file rest)
            "gen" (bookmarks-generate bookmark-file rest)
            (println (str usage-add "\n" usage-gen))))))
