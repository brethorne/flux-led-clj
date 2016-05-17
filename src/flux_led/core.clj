(ns flux-led.core
  (:require [clojure.string :refer [join split]]
            [clojure.java.io :refer [output-stream input-stream]]
            [clj-time.core :as t]
            [clj-time.local :as l])
  (:gen-class)
  (:import (java.net DatagramSocket DatagramPacket InetAddress SocketTimeoutException NetworkInterface Socket)))

;; THOUGHTS
;; 1. Use protocols? A lightbulb is just an ip and a port
;; 2. Probably don't need two macros for sending commands. Just ignore
;;    the response if you don't care about it.
;; 3. Structure the sending and reading of commands to do the broadcast
;;    the same way I'm doing it by sending run of the mill commands.
;;    Possibly reuse the same macro although that might be a pain
;;    since one uses a DatagramSocket and the other a regular Socket
;; 4. I bet there are really good opportunities to use juxt
;; 5. When scanning, need to make the delay a little bit longer. It
;;    doesn't find it if the lightbulb's been sleeping for a while and
;;    it needs more time to wake up
;; 6. Every function that has to transform bytes should not be also unsigning the bytes
;;    in the fn, that should happen centrally and then the transform fns should just
;;    be passed a vector of byte values
;; 7. Use the gensym shorthand for the let bindings in my macro

(def colors {:red    [255 0 0]
             :green  [0 128 0]
             :yellow [255 255 0]
             :purple [128 0 128]
             :blue   [0 0 205]
             :indigo [75 0 130]
             :orange [255 165 0]
             :brown  [165 42 42]
             :pink [255 192 203]
             :white [255 255 255]})

(def default-port 5577)

(def patterns {0x25 :seven-color-cross-fade
               0x26 :red-gradual-change
               0x27 :green-gradual-change
               0x28 :blue-gradual-change
               0x29 :yellow-gradual-change
               0x2A :cyan-gradual-change
               0x2B :purple-gradual-change
               0x2C :white-gradual-change
               0x2D :red-green-cross-fade
               0x2E :red-blue-cross-fade
               0x2F :green-blue-cross-fade
               0x30 :seven-color-strobe-flash
               0x31 :red-strobe-flash
               0x32 :green-strobe-flash
               0x33 :blue-strobe-flash
               0x34 :yellow-strobe-flash
               0x35 :cyan-strobe-flash
               0x36 :purple-strobe-flash
               0x37 :white-strobe-flash
               0x38 :seven-color-jumping})

(defn broadcasts []
  (->> (enumeration-seq (NetworkInterface/getNetworkInterfaces))
       (filter #(and (.isUp %) (not (.isLoopback %))))
       (mapcat #(.getInterfaceAddresses %))
       (map #(.getBroadcast %))
       (filter some?)))

(defn datagram-socket [timeout]
  (doto (DatagramSocket.) (.setBroadcast true) (.setSoTimeout timeout)))

(defn datagram-packet
  ([] (datagram-packet 64))
  ([len] (DatagramPacket. (byte-array len) len))
  ([message ^InetAddress inet port] (DatagramPacket. (.getBytes message) (count message) inet port)))

(defn send-message [message ^InetAddress inet port timeout]
  (let [socket (datagram-socket timeout)]
    (do
      (.send socket (datagram-packet message inet port))
      socket)))

(defn send-broadcast-messages []
  (let [message "HF-A11ASSISTHREAD"
        port 48899
        timeout 1000]
    (map #(send-message message % port timeout) (broadcasts))))

(defn wait-for-data [socket timeout]
  (loop [data #{}
         receive-packet (datagram-packet)
         time-start (System/currentTimeMillis)]
    (if (> (- (System/currentTimeMillis) time-start) timeout)
      data
      (do
        (try (.receive socket receive-packet)
             (catch SocketTimeoutException _))
        (recur (conj data (String. (.getData receive-packet)))
               (datagram-packet)
               time-start)))))

(defn parse-recv-message [message]
  (let [[ip id model] (split message #",")]
    {:ip ip :id id :model model}))

(defn receive-broadcast-messages [socket]
  (let [raw-messages (wait-for-data socket 3000)]
    (->> raw-messages
         (map #(remove (comp zero? int) %))
         (filter seq)
         (map join)
         (map parse-recv-message))))

(defn scan []
  (let [sockets (send-broadcast-messages)]
    (try (mapcat receive-broadcast-messages sockets)
         (finally (doall (map #(.close %) sockets))))))

(defn with-checksum [bytes]
  (let [bytes-vec (vec bytes)
        cs (reduce + bytes-vec)]
    (byte-array (concat bytes-vec [cs]))))

(defmacro send-cmd [ip port cmd]
  (let [cmd-sym (gensym)
        os (gensym)
        socket (gensym)]
    `(let [~cmd-sym (with-checksum (byte-array ~cmd))]
       (with-open [~socket (doto (Socket. ~ip ~port) (.setSoTimeout 1000))
                   ~os (output-stream ~socket)]
         (.write ~os ~cmd-sym)))))

(defmacro send-cmd-with-resp [ip port cmd len]
  (let [cmd-sym (gensym)
        is (gensym)
        os (gensym)
        socket (gensym)
        buf (gensym)]
    `(let [~cmd-sym (with-checksum (byte-array ~cmd))]
       (with-open [~socket (doto (Socket. ~ip ~port) (.setSoTimeout 1000))
                   ~is (input-stream ~socket)
                   ~os (output-stream ~socket)]
         (let [~buf (byte-array ~len)]
           (.write ~os ~cmd-sym)
           (.flush ~os)
           (.read ~is ~buf 0 ~len)
           ~buf)))))

(defn power [ip port on]
  (let [on-off (if on 0x23 0x24)
        cmd [0x71 on-off 0x0F]]
    (send-cmd ip port cmd)))

(defn turn-on
  ([ip] (turn-on ip default-port))
  ([ip port] (power ip port true)))

(defn turn-off
  ([ip] (turn-off ip default-port))
  ([ip port] (power ip port false)))

(defn fit-between [n min max]
  (cond (< n min) min
        (> n max) max
        :else n))

(defn percent->byte [percent]
  (let [normalized (fit-between percent 0 100)]
    (int (/ (* normalized 255) 100))))

(defn rgb
  ([ip [r g b]]
   (rgb ip default-port [r g b] true))
  ([ip port [r g b]]
   (rgb ip port [r g b] true))
  ([ip port [r g b] persist]
   (let [pb (if persist 0x31 0x41)]
     (send-cmd ip port [pb r g b 0x00 0xF0 0x0F]))))

(defn warm-white
  ([ip percent] (warm-white ip default-port percent true))
  ([ip port percent] (warm-white ip port percent true))
  ([ip port percent persist]
   (let [pb (if persist 0x31 0x41)
         pctb (percent->byte percent)]
     (send-cmd ip port [pb 0x00 0x00 0x00 pctb 0x0F 0x0F]))))

(defn uaget [^bytes array idx]
  (bit-and (aget array idx) 0xFF))

(defn power-state [resp]
  (condp = (uaget resp 2)
    0x23 :on
    0x24 :off
    :unknown))

(defn valid? [pattern]
  (let [pattern-ids (set (keys patterns))]
    (pattern-ids pattern)))

(defn bulb-mode-state [resp]
  (let [pattern (uaget resp 3)
        ww-level (uaget resp 9)]
    (cond
      (#{0x61 0x62} pattern) (if (not (zero? ww-level)) :warm-white :color)
      (= 0x60 pattern) :custom
      (valid? pattern) :preset
      :else :unknown)))

(defn pattern-speed-state [resp]
  (let [raw-delay (dec (uaget resp 5))
        max-delay (dec 0x1F)
        delay (fit-between raw-delay 0 max-delay)]
    (- 100 (int (/ (* delay 100) max-delay)))))

(defn rgb-state [resp]
  (mapv #(uaget resp %) (range 6 (inc 8))))

(defn warm-white-pct-state [resp]
  (let [ww-raw (uaget resp 9)
        ww-pct (fit-between ww-raw 0 255)]
    (int (Math/ceil (/ (* ww-pct 100) 255)))))

(defn pattern-state [resp]
  (let [pattern (uaget resp 3)]
    (get patterns pattern)))

(defn bulb-state
  ([ip] (bulb-state ip default-port))
  ([ip port]
   (let [resp (send-cmd-with-resp ip port [0x81 0x8A 0x8B] 14)]
     {:power          (power-state resp)
      :mode           (bulb-mode-state resp)
      :speed          (pattern-speed-state resp)
      :rgb            (rgb-state resp)
      :warm-white-pct (warm-white-pct-state resp)
      :pattern        (pattern-state resp)})))

(defn clock
  ([ip] (clock ip default-port))
  ([ip port]
   (let [resp (send-cmd-with-resp ip port [0x11 0x1A 0x1B 0x0F] 12)]
     (-> (zipmap [:year :month :day :hour :minute :second]
                 (map #(uaget resp %) (range 3 (inc 8))))
         (update :year #(+ 2000 %))))))

(defn synchronize-clock
  ([ip] (synchronize-clock ip default-port))
  ([ip port]
   (let [cmd [0x10 0x14]
         end [0x00 0x0F]
         ts (-> ((juxt t/year t/month t/day t/hour t/minute t/second t/day-of-week) (l/local-now))
                (update 0 #(- % 2000)))]
     (send-cmd ip port (concat cmd ts end)))))

;; LEFT OFF ON WARMTH LEVEL
(defn bytes->timer [bytes]
  {:active?      (= 0xF0 (nth bytes 0))
   :year         (+ (nth bytes 1) 2000)
   :month        (nth bytes 2)
   :day          (nth bytes 3)
   :hour         (nth bytes 4)
   :minute       (nth bytes 5)
   :repeat-mask  (nth bytes 7)
   :pattern-code (nth bytes 8)
   :mode         (condp = (nth bytes 8)
                   0x61 :color
                   0x00 :default
                   :preset)
   :rgb          (mapv #(nth bytes %) [9 10 11])
   :delay        (nth bytes 9)})

(defn timers [ip port]
  (let [resp-len 88
        timer-len 14
        resp (send-cmd-with-resp ip port [0x22 0x2A 0x2B 0x0F] resp-len)]
    (->> (partition timer-len (range 2 (inc resp-len)))
         (map (fn [offsets]
                (mapv #(uaget resp %) offsets)))
         (map bytes->timer))))

(defn flash [ip len]
  (let [rgb-fn (fn [] (rgb ip (:blue colors)))
        off-fn (fn [] (rgb ip [0 0 0]))
        sleep-fn (fn [] (Thread/sleep 1000))]
    (map (fn [f]
           (f))
         (interpose sleep-fn (flatten (repeat [rgb-fn off-fn]))))))
