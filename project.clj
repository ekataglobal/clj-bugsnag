(defproject whitepages/clj-bugsnag "0.3.0"
  :description "Fully fledged Bugsnag client. Supports ex-data and ring middleware."
  :url "https://github.com/whitepages/clj-bugsnag"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.3.0"
  :dependencies [
    [org.clojure/clojure "1.8.0"]
    [clj-stacktrace/clj-stacktrace "0.2.8"]
    [clj-http/clj-http "2.0.0"]
    [cheshire/cheshire "5.5.0"]
    [environ/environ "1.0.2"]
    [org.clojure/data.json "0.2.6"]]

  :aliases {
    "test" ["midje"]}
  :profiles {
    :dev {
      :dependencies [[midje/midje "1.9.9" :exclusions [potemkin riddley]]]
      :plugins      [[lein-midje/lein-midje "3.2.1"]]}})
