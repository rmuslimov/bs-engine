(defproject bs-engine "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.namespace "0.2.11"]]
  :main ^:skip-aot bs-engine.core
  :target-path "target/%s"
  :profiles {:dev
             {:source-paths ["dev"]}
             :uberjar {:aot :all}})