(ns bookmarks
  (:require [net.cgrand.enlive-html :as html])
  (:use [clojure.set :only [union]])
  (:import java.text.SimpleDateFormat)
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

(defn augment-untagged [link] (if (empty? (:tags link))
                                (assoc link :tags #{:untagged}) link))

(defn individual-bookmark-tags [template]
  (html/snippet
   template
   [:#timewise :> html/first-child :> html/first-child :ul :> html/first-child]
   [tag]
   [:li :a] (html/do-> (html/content tag)
                       (html/set-attr :href (str "#tag-" tag)))))

(defn individual-bookmark [template ind-tags-snippet]
  (html/snippet
   template
   [:#timewise :> html/first-child :> html/first-child]
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
     template [bookmarks tags date]
     [:#datestamp]    (html/content date)
     [:#timewise :ul] (html/content (map ind-bm-snippet bookmarks))
     [:#tagtoc :ul]   (html/content (map (tag-toc template bookmarks) tags))
     [:#tagwise]      (html/content
                       (map (tagwise template bookmarks ind-bm-snippet) tags)))))

(defn bookmarks-generate
  ([file [template output & rest]]
     (let [bookmarks (read-string (slurp file))]
       (if-not (and template output)
         (println usage-gen)
         (->> [bookmarks
               (tags-from-bookmarks bookmarks)
               (. (SimpleDateFormat. "yyyy/MM/dd") format (java.util.Date.))]
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
