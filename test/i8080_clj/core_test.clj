(ns i8080-clj.core-test
  (:require
    [i8080-clj.core :refer [initial-state]]
    [i8080-clj.disassemble :as dis]
    [i8080-clj.rom :as rom]))

#_
(rom/dump-rom initial-state "invaders.rom")

#_
(let [{:keys [pc] :as state} (rom/load-rom initial-state "invaders.rom")]
  (dissoc
    (reduce (fn [st _] (dis/disassemble-op st)) state (range 5))
    :mem))

