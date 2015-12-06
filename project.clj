(defproject iiqm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.finger-tree "0.0.2"]
                 [spyscope "0.1.5"]
                 [org.clojure/test.check "0.9.0"]]
  :main ^:skip-aot iiqm.core
  :target-path "target/%s"
  :plugins [[lein-gorilla "0.3.5"]]
  :profiles {:uberjar {:aot :all}})
