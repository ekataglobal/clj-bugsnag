

# clj-bugsnag

A fully fledged [Bugsnag](https://bugsnag.com) exception reporting client for Clojure.


## Features

 - Automatically exposes ex-info data as metadata
 - Ring middleware included, attaches ring request map as metadata
 - Include snippet of code around stack trace lines
 - Mark in-project stack traces to hide frameworks
 - Pass along user IDs to Bugsnag
 - Tested, used in production at [6 Wunderkinder](http://www.6wunderkinder.com/) for [Wunderlist](https://www.wunderlist.com/)


## Releases and Dependency Information

The latest version of clj-bugsnag is currently only available via [git-deps](https://clojure.org/guides/deps_and_cli#_using_git_libraries),
but there is an older version, 0.4.0, available via [Clojars](https://clojars.org/whitepages/clj-bugsnag).


## Example Usage

```clojure
(require '[clj-bugsnag.core :as bugsnag]
         '[clj-bugsnag.ring :as bugsnag.ring])

;; Ring middleware, all keys besides :api-key are optional:

(bugsnag.ring/wrap-bugsnag
  handler
  {:api-key "Project API key"
   ;; Defaults to "production"
   :environment "production"
   ;; Project namespace prefix, used to hide irrelevant stack trace elements
   :project-ns "your-project-ns-prefix"
   ;; A optional version for your app, this is displayed in bugsnag.
   ;; If not provided the latest git sha will be used - this means that
   ;; the git repo is available when you run your app.
   :version "your-app-version"
   ;; A optional function to extract a user object from a ring request map
   ;; Used to count how many users are affected by a crash
   :user-from-request (constantly {:id "shall return a map"})})

;; Manual reporting using the notify function:

(try
  (some-function-that-could-crash some-input)
  (catch Exception exception

    ;; Notify with options map, all keys are optional:
    (bugsnag/notify
      exception
      {:api-key "Project API key"
       ;; Attach custom metadata to create tabs in Bugsnag:
       :meta {:input some-input}
       ;; Pass a user object to Bugsnag for better stats
       :user {:id ... :email ...}})

    ;; If no api-key is provided, clj-bugsnag
    ;; will fall back to BUGSNAG_KEY environment variable
    (bugsnag/notify exception)))
```


## Unit Tests

Running `bin/kaocha` runs all tests using the [kaocha-midje](https://github.com/lambdaisland/kaocha-midje)
kaocha plugin.
There is only output when there are failed tests.

## License

Copyright © 2014-2015 6 Wunderkinder GmbH.

Distributed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html), the same as Clojure.
