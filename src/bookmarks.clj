(ns bookmarks
  (:require [net.cgrand.enlive-html :as html]
            [clojure.contrib.str-utils2 :as str]
            [clojure.set :as set])
  (:import java.io.File
           java.util.Date
           java.text.DateFormat)
  (:gen-class))

(defn tags-from-bookmarks [bookmarks]
  (conj (->> bookmarks
             (map #(get % :tags))
             (apply set/union)
             (sort))
        :untagged))

(defn tag-string [tag] (apply str (rest (str tag))))

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
                            (map tag-string)
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
                (html/set-attr :id (str "count-" (tag-string tag))))
   [:li :a] (let [s (tag-string tag)]
             (html/do-> (html/content s)
                        (html/set-attr :href (str "#tag-" s))))))

(defn subset-list [template bookmarks link-list]
  (html/snippet
   template
   [:#tagwise :> html/first-child]
   [tag]
   [:h2] (let [s (tag-string tag)]
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

(defn -main [& args]
  (let [[template bookmark-file output & rest] args]
    (if-not output
      (println "Usage: java -jar standalone-jar template bookmarks output")
      (let [bookmarks (load-string (str "[" (slurp bookmark-file) "]"))]
        (->>
         [bookmarks
          (tags-from-bookmarks bookmarks)
          (. (DateFormat/getDateInstance DateFormat/SHORT)
             format (Date.))]
         (apply (template-from-file (File. template)))
         (apply str)
         (spit output))))))

