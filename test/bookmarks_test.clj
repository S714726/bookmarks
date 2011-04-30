(ns bookmarks-test
  (:use bookmarks)
  (:use clojure.test)
  (:require [net.cgrand.enlive-html :as html])
  (:import java.io.File)
  (:import java.io.StringReader))

;;; Run output from the program through again as a template with the same
;;;   bookmark set
(deftest homomorphism
  (let [bookmarks (load-string (str "[" (slurp "example/marks.clj") "]"))
        args [bookmarks (tags-from-bookmarks bookmarks) "datestamp"]
        original (apply (template-from-file (File. "example/template.html"))
                        args)]
    (is (= (apply str original)
           (apply str (apply (->> original
                                  (apply str)
                                  (StringReader.)
                                  (html/html-resource)
                                  (template-from-file))
                             args))))))
