(ns clj-bugsnag.core
  (:require [clj-http.client :as http]
            [clj-stacktrace
             [core :refer [parse-exception]]
             [repl :refer [method-str]]]
            [clojure
             [repl :as repl]
             [string :as string]
             [walk :as walk]]
            [clojure.java.shell :refer [sh]]
            [environ.core :refer [env]]))

(def git-rev
  (delay
   (try
     (string/trim (:out (sh "git" "rev-parse" "HEAD")))
     (catch Exception ex "git revision not available"))))

(defn- find-source-snippet
  [around, function-name]
  (try
    (let [fn-sym        (symbol function-name)
          fn-var        (find-var fn-sym)
          source        (repl/source-fn fn-sym)
          start         (-> fn-var meta :line)
          indexed-lines (map-indexed (fn [i, line]
                                       [(+ i start) (string/trimr line)])
                                     (string/split-lines source))]
      (into {} (filter #(<= (- around 3) (first %) (+ around 3)) indexed-lines)))
    (catch Exception ex
      nil)))

(defn- transform-stacktrace
  [trace-elems project-ns]
  (try
    (vec (for [{:keys [file line ns]
                :as   elem} trace-elems
               :let         [project? (string/starts-with? (or ns "_") project-ns)
                             method   (method-str elem)
                             code     (when (string/ends-with? (or file "") ".clj")
                                        (find-source-snippet line (string/replace (or method "") "[fn]" "")))]]
           {:file       file
            :lineNumber line
            :method     method
            :inProject  project?
            :code       code}))
    (catch Exception ex
      [{:file       "clj-bugsnag/core.clj"
        :lineNumber 1
        :code       {1 (str ex)
                     2 "thrown while building stack trace."}}])))

(defn- stringify
  [thing]
  (if (or (map? thing) (string? thing) (number? thing) (nil? thing) (true? thing) (false? thing) (sequential? thing))
    thing
    (str thing)))

(defn- unroll [ex project-ns]
  (loop [collected []
         current   ex]
    (let [class-name (.getName ^Class (:class current))
          stacktrace (transform-stacktrace (:trace-elems current) project-ns)
          new-item   {:errorClass class-name
                      :message    (:message current)
                      :stacktrace stacktrace}
          collected  (conj collected new-item)]
      (if-let [next (:cause current)]
        (recur collected next)
        collected))))

(defn exception->json
  [exception options]
  (let [ex         (parse-exception exception)
        class-name (.getName ^Class (:class ex))
        project-ns (get options :project-ns "\000")
        base-meta  (if-let [d (ex-data exception)]
                     {"ex–data" d}
                     {})]
    {:apiKey   (:api-key options (env :bugsnag-key))
     :notifier {:name    "clj-bugsnag"
                :version "0.3.0"
                :url     "https://github.com/whitepages/clj-bugsnag"}
     :events   [{:payloadVersion "2"
                 :exceptions     (unroll ex project-ns)
                 :context        (:context options)
                 :groupingHash   (if-let [group-fn (:group-fn options)]
                                   (group-fn ex)
                                   (:group options))
                 :severity       (or (:severity options) "error")
                 :user           (:user options)
                 :app            {:version      (if (contains? options :version)
                                                  (:version options)
                                                  @git-rev)
                                  :releaseStage (or (:environment options) "production")}
                 :device         {:hostname (.. java.net.InetAddress getLocalHost getHostName)}
                 :metaData       (walk/postwalk stringify (merge base-meta (:meta options)))}]}))

(defn notify
  "Main interface for manually reporting exceptions.
   When not :api-key is provided in options,
   tries to load BUGSNAG_KEY var from enviroment."
  ([exception]
   (notify exception nil))
  ([exception, options]
   (let [params (exception->json exception options)
         url    "https://notify.bugsnag.com/"]
     (http/post url {:form-params  params
                     :content-type :json}))))
