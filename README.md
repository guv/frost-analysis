# frost-analysis

A Clojure library for analyzing the serialization statistics when serializing Clojure data with [frost](https://github.com/guv/frost).

## Install

Add the following to your dependency vector in your project.clj:

```clojure
[frost-analysis "0.1.0"]
```

Latest on [clojars.org](http://clojars.org):

![Version](https://clojars.org/frost-analysis/latest-version.svg)


## Usage

```clojure
(require
  '[frost.analysis :as a]
  '[frost.quick-freeze :as qf])
  
(a/with-serialization-analysis
  (def byte-data (let [data (mapv (fn [n] (zipmap (range n) (mapv (comp keyword str) (range n)))) (range 100))]
                   (qf/quick-byte-freeze data :analyze true))))
```



## License

Copyright © 2017 Gunnar Völkel

Distributed under the Eclipse Public License version 1.0
