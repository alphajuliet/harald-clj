;; game.clj

(ns harald.actions
  (:require [harald.state :refer :all]
            [harald.actions :refer :all]
            [harald.hash-calc :as h]
            [random-seed.core :as r]
            [clojure.java.io :as io])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

;;-------------------------------
;; Logging setup

(def log-file-name "game.txt")
(io/delete-file log-file-name :quiet)

(defn- spit-seq
  "Convert a sequence to a string in parentheses."
  [dest s]
  (spit dest
        (str "(" (clojure.string/join " " s) ")\n")
        :append true))

(defn log
  "Print a string to a log file."
  [s]
  (if (seq? s)
    (spit-seq log-file-name s)
    (spit log-file-name (str s "\n")
          :append true)))

;; The End