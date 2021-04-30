(ns join-leave.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.core.async :refer [chan close! go <!]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [discljord.events :refer [message-pump!]]
            [discljord.formatting :refer [user-tag]]))

(def state (atom nil))

(def bot-id (atom nil))

(def cache (atom {}))

(def config (edn/read-string (slurp "config.edn")))

(def greetings-hi [
                   ""])

(defmulti handle-event (fn [type _data] type))

(defmethod handle-event :ready
  [_ _]
  (discord-ws/status-update! (:gateway @state) :activity (discord-ws/create-activity :name (:playing config))))

(defmethod handle-event :default [_ _])

(defmethod handle-event :guild-member-add
  [_ {:keys [guild-id user]}]
  (go (let [channels (if (contains? @cache guild-id) 
                       (get @cache guild-id) 
                       (let [channels-to-cache (discord-rest/get-guild-channels! (:rest @state) guild-id)]
                         (swap! cache assoc guild-id channels-to-cache)
                         channels-to-cache))
            text-channels (filter #(= (:type %) 0) (<! channels))
            valid-names #{"join-spam" "join-leave" "hi" "welcome"}
            channel-id (:id (first (filter (comp valid-names :name) text-channels)))]
        (discord-rest/create-message! (:rest @state) channel-id
                                      :embed {:title (str "Hi, " (user-tag user))}))))

(defmethod handle-event :guild-member-remove
  [_ {:keys [guild-id user]}]
  (go (let [channels (if (contains? @cache guild-id)
                       (get @cache guild-id)
                       (let [channels-to-cache (discord-rest/get-guild-channels! (:rest @state) guild-id)]
                         (swap! cache assoc guild-id channels-to-cache)
                         channels-to-cache))
            text-channels (filter #(= (:type %) 0) (<! channels))
            valid-names #{"join-spam" "join-leave" "hi" "welcome"}
            channel-id (:id (first (filter (comp valid-names :name) text-channels)))]
        (discord-rest/create-message! (:rest @state) channel-id
                                      :embed {:title (str "Bye, " (user-tag user))}))))

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (discord-ws/disconnect-bot! gateway)
  (close! events))

(defn -main [& args]
  (reset! state (start-bot! (:token config) :guild-messages :guild-members))
  (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
  (try
    (message-pump! (:events @state) handle-event)
    (finally (stop-bot! @state))))

