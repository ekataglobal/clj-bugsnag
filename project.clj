(defproject clj-bugsnag "0.1.2"
  :description "Fully fledged Bugsnag client. Supports ex-data and ring middleware."
  :url "http://github.com/6wunderkinder/clj-bugsnag"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.3.0"
  :dependencies [
    [clj-stacktrace "0.2.7"]
    [clj-http "0.9.2"]
    [org.clojure/data.json "0.2.4"]
    [org.clojure/clojure "1.6.0"]]
  :profiles {
    :dev {
      :dependencies [[midje "1.6.3"]]
      :plugins      [[lein-midje "3.1.3"]]}})
