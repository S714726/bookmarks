(ns bookmarks
  (:require [net.cgrand.enlive-html :as html])
  (:use [clojure.set :only [union]])
  (:import java.text.DateFormat)
  (:gen-class))

(def usage-base "Usage: java -jar bookmarks-latest.jar BOOKMARKS ")
(def usage-add (str usage-base "add LINK [-t TITLE] [TAGS]"))
(def usage-gen (str usage-base "gen TEMPLATE"))

(defn tags-from-bookmarks [bookmarks]
  (conj (->> bookmarks
             (map #(get % :tags))
             (apply union)
             (sort))
        :untagged))

(defn augment-untagged [link] (if (empty? (:tags link))
                                (assoc link :tags #{:untagged}) link))

(defn bookmark-tags [template]
  (html/snippet
   template
   [:#timewise :> html/first-child :> html/first-child :ul :> html/first-child]
   [tag]
   [:li :a] (html/do-> (html/content tag)
                       (html/set-attr :href (str "#tag-" tag)))))

(defn bookmark-list [template tag-list]
  (html/snippet
   template
   [:#timewise :> html/first-child :> html/first-child]
   [{:keys [title link tags]}]
   [:div.desc :a] (html/do-> (html/content title)
                             (html/set-attr :href link))
   [:div.url :a] (html/do-> (html/content link)
                            (html/set-attr :href link))
   [:ul] (html/content (->> tags
                            (map name)
                            (map tag-list)))))

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

(defn subset-list [template bookmarks link-list]
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
                            (map link-list)))))

(defn template-from-file [template]
  (let [blsnip (bookmark-list template (bookmark-tags template))]
    (html/template
     template [bookmarks tags date]
     [:#datestamp]    (html/content date)
     [:#timewise :ul] (html/content (map blsnip bookmarks))
     [:#tagtoc :ul]   (html/content (map (tag-toc template bookmarks) tags))
     [:#tagwise]      (html/content
                       (map (subset-list template bookmarks blsnip) tags)))))

(defn bookmarks-generate
  ([bookmarks [template & rest]]
     (if-not template
       (println usage-gen)
       (->> [bookmarks
             (tags-from-bookmarks bookmarks)
             (. (DateFormat/getDateInstance DateFormat/SHORT)
                format (java.util.Date.))]
            (apply (template-from-file (java.io.File. template)))
            (apply str)
            (println)))))


(defn title-from-web [link]
  (try
    (->> (html/select (html/html-resource (java.net.URL. link))
                      #{[:head :title] [:h1]})
         (map html/text)
         (first))
    ;; MalformedURL, UnknownHost, etc.
    (catch Exception ex link)))

(defn bookmarks-add
  ([bookmarks [link title-flag title-opt & rest]]
     (if-not link
       (println usage-add)
       (let [[title tags] (if (= title-flag "-t")
                            [title-opt rest]
                            [(title-from-web link)
                             (remove nil? (conj rest title-flag title-opt))])]
         (prn (conj bookmarks
                    {:title title, :link link
                     :tags (set (map keyword tags))}))))))


(defn -main [& args]
  (let [[bookmark-file operation & rest] args]
    (if-not bookmark-file
      (println (str usage-add "\n" usage-gen))
      (let [bookmarks (read-string (slurp bookmark-file))]
        (case operation
              "add" (bookmarks-add bookmarks rest)
              "gen" (bookmarks-generate bookmarks rest)
              (println (str usage-add "\n" usage-gen)))))))
