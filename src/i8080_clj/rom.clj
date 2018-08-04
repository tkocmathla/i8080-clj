(ns i8080-clj.rom
  (:require
    [clojure.java.io :as io]
    [i8080-clj.disassemble :refer [disassemble-op]]))

(defn read-binary-file
  "Reads slurpable file into a vector of bytes"
  [file]
  (with-open [in (io/input-stream file)]
    (let [len (.length (io/file file))
          buf (byte-array len)]
      (.read in buf 0 len)
      (vec buf))))

(defn load-rom
  [state rom]
  (assoc state :mem (read-binary-file (io/resource rom))))

(defn dump-rom
  "Prints every instruction and its args found in rom file"
  [state rom]
  (loop [{:keys [pc] :as state} (load-rom state rom)]
    (let [{:keys [size args] :as op} (disassemble-op state)]
      (when size
        (println (format "%04x" pc) (dissoc op :f))
        (recur (update state :pc + size))))))
