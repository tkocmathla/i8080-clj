(ns i8080-clj.rom
  (:require [clojure.java.io :as io]))

(defn read-binary-file
  "Reads slurpable file into a vector of bytes"
  [file]
  (with-open [in (io/input-stream file)]
    (let [len (.length (io/file file))
          buf (byte-array len)]
      (.read in buf 0 len)
      (mapv (partial bit-and 0xff) buf))))

(defn load-rom
  [mem rom]
  (let [mem-len (count mem)
        rom (read-binary-file (io/resource rom))
        rom-len (count rom)]
    (doseq [i (range rom-len)]
      (aset-int mem i (rom i)))
    mem))
