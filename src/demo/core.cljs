(ns demo.core
    (:require
      [clojure.string :as str]
      [demo.web3 :as web3]
      [reagent.core :as r]
      [reagent.dom :as d]
      [goog.labs.format.csv :as csv]
      [cljs.core.async :refer [<! >! put! chan go go-loop] :as a]
      [cljs.core.async.interop :refer-macros [<p!]]
      ;;["./artifacts/deployments/map.json" :as m]
      ))

(enable-console-print!)

(def w3 (r/atom
         {:provider nil
          :signer nil
          :account ""
          :balance 0
          :readable-abi []}))

(defn handle-ethereum
  []
  (let [detected web3/is-metamask-installed]
    (if detected
      (do
        (println "metamask is detected")
        (let [provider (web3/get-provider)
              signer (.getSigner provider)]
          (reset! w3 {:provider provider
                      :signer signer
                      :account ""
                      :balance 0
                      :readable-abi []})))
      (println "plz install metamask"))
    (js/console.log "w3 state : " w3)))

(defn load-event []
  (println "Page loaded!")
  (.addEventListener js/window "ethereum#initialized" handle-ethereum {:once true})
  (js/setTimeout handle-ethereum 3000))

;; event listener is not reliable
;;(.addEventListener js/window "load" load-event false)

(defn doc-ready-handler []
  (let[ ready-state (. js/document -readyState)]
    (if (= "complete" ready-state)
      (do
        (println "document ready")
        (handle-ethereum)))))

(defn on-doc-ready []
  (set! (.-onreadystatechange js/document) doc-ready-handler))

(on-doc-ready)

(defn chain-change-handler
  [e]
  (.reload js/window.location))

(if web3/is-metamask-installed
  (.on web3/ethereum "chainChanged" chain-change-handler))

;; -------------------------
;; Views
(def default-app-state {:abi-data []
                        :abi-file-name nil
                        :tx-inputs {}
                        :tx-result nil
                        :contract nil
                        :contract-addr "0x5a30d9b2e3a404d89e7c75491dca0d3e279e14d7"})
(def uniq-key (r/atom 0))
(def app-state (r/atom default-app-state))
(def addr (r/atom "0x..."))

(defn gen-key! []
  (let [next (swap! uniq-key inc)]
    next))

(defonce blockheight (r/atom 0))
(defn update-blockheight-handler
  [e]
  (let [provider (:provider @w3)]
    (when (not (nil? provider))
      (go
        (let [r (<p! (.getBlockNumber provider))]
          (reset! blockheight r))))))

(defonce block-updater (js/setInterval update-blockheight-handler 5000))

(def first-file
  "Accepts input change events and gets the first selected file."
  (map (fn [e]
         (let [target (.-currentTarget e)
               file (-> target .-files (aget 0))]
           (set! (.-value target) "")
           file))))

(def extract-result
  "Accepts a FileReader onload event, gets the parsed result."
  (map #(-> % .-target .-result js/JSON.parse js->clj )))

(def upload-reqs (chan 1 first-file))
(def file-reads (chan 1 extract-result))

(defn put-upload! [e]
  (put! upload-reqs e))

(go-loop []
       (let [reader (js/FileReader.)
             file (<! upload-reqs)]
         (swap! app-state assoc :abi-file-name (.-name file))
         (set! (.-onload reader) #(put! file-reads %))
         (.readAsText reader file))
       (recur))

(go-loop []
  (let [abi-data (.get (<! file-reads) "abi")
        contract-addr (:contract-addr @app-state)
        provider (:provider @w3)
        contract (web3/get-contract contract-addr abi-data provider)]
    (swap! app-state assoc :abi-data abi-data :contract contract)
    (recur)))

(defn upload-btn [abi-file-name]
  [:div
   (or abi-file-name "upload abi : ")
   [:input {:type "file"
            :name "inputfile"
            :id "inputfile"
            :accept ".json"
            :on-change put-upload!
            }]
   (when abi-file-name
     [:input {:type "button"
              :style {:background-color "pink"}
              :on-click #((reset! app-state default-app-state)
                          (reset! uniq-key 0))
              :value "reset"}])])

(defn read-func? [meta]
  (let [{ftype "type"
         visible "stateMutability"} meta]
    (and (= ftype "function")
         (contains? #{"view" "pure"} visible))))

(defn write-func? [meta]
  (let [{ftype "type"
         visible "stateMutability"} meta]
    (and (= ftype "function")
         (not (contains? #{"view" "pure"} visible)))))

(defn input-converter [itype]
  (case itype
    "address" identity
    "uint256" js/parseInt
    "uint128" js/parseInt
    "bool" #(if (= % "true") true false)
    str))

(defn output-converter [itype]
  (case itype
    "address" identity
    "uint256" #(.toString %)
    "unit128" #(.toString %)
    "bool" #(if (= % "true") true false)
    str))

(defn tx-input-component
  [fname inputs outputs]
  [:<>
   (let [tx-inputs (:tx-inputs @app-state)]
     (doall
      (for [input inputs
            :let [{iname "name"
                   itype "type"} input
                  ;;_ (js/console.log "input field: " iname "/" itype "/")
                  ifield (str iname "(" itype ")")]]
        [:span {:key ifield}
         [:label {:for (str fname ifield "text")} ifield]
         [:input {:id (str fname ifield "text")
                  :type "text"
                  :placeholder ifield
                  :value (get-in tx-inputs [fname iname])
                  :on-change (fn [e]
                               (let [cvt (input-converter itype)
                                     new-value (cvt (.. e -target -value))]
                                 (swap! app-state assoc-in [:tx-inputs fname iname] new-value)))}]])))
   (doall
    (for [output outputs]
      (let [{otype "type"} output
            ;;_ (js/console.log "output field: " output)
            ;;_ (js/console.log "output type: " otype)
            ]
        [:p {:key (gen-key!)}
         (str "|_ " otype)])))])

(defn tx-btn-component
  [btn-name view? fname inputs outputs]
  (let [tx-inputs (:tx-inputs @app-state)]
    [:input {:type "button"
             :value btn-name
             :on-click (fn [e]
                         (go
                           (let [_contract (:contract @app-state)
                                 contract (if view?
                                            _contract
                                            (.connect _contract (:signer @w3)))
                                 tx-input-map (get tx-inputs fname)
                                 input-args-order (map #(get % "name") inputs)
                                 tx-input (if (zero? (count input-args-order))
                                            [nil]
                                            (map #(get tx-input-map %) input-args-order))
                                 tx-result (<p! (web3/call contract fname tx-input))
                                 tx-results (if (seq? tx-result) tx-result [tx-result])
                                 output-types (map #(get % "type") outputs)
                                 cvt-result (#(for [otype %1
                                                    output %2]
                                                ((output-converter otype) output)) output-types tx-results)]
                             (js/console.log "input args order : " input-args-order)
                             (js/console.log "input args : " tx-input-map)
                             (js/console.log "converted input : " tx-input)
                             (js/console.log "tx results : " tx-results)
                             (js/console.log "converted result : " cvt-result)
                             (swap! app-state assoc-in [:tx-result fname] tx-results)
                             (swap! app-state assoc-in [:cvt-result fname] cvt-result)
                             )))}]))

(defn tx-output-component
  [fname outputs]
  [:ul
   (let [tx-inputs (:tx-inputs @app-state)
         tx-result (get-in @app-state [:tx-result fname])
         cvt-result (get-in @app-state [:cvt-result fname])
         ]
     (if (empty? outputs)
       [:samp (str (js->clj tx-result))]
       (for [res cvt-result
             {otype "type"} outputs]
         ^{:key (gen-key!)}
         [:li
          [:p (str otype ": " res)]])))])


(defn function [meta view?]
  (let [{fname "name"
         inputs "inputs"
         outputs "outputs"} meta
        ;;_ (js/console.log "render func: " fname "/" inputs)
        btn-name (if view? "query" "write")]
    [:li
     [:p [:b fname]]
     [tx-input-component fname inputs outputs]
     [tx-output-component fname outputs]
     [tx-btn-component btn-name view? fname inputs outputs]]))

(defn contract-funcs [funcs view?]
  [:div {:style {:border-style "ridge"}}
   [:b (if view? "read contract" "write contract")]
   [:ul
      (for [func funcs]
        ^{:key (gen-key!)}
        [function func view?])]])

(defn result [metas]
  (let [read-funcs (filter read-func? metas)
        write-funcs (filter write-func? metas)]
    [:<>
     [contract-funcs read-funcs true]
     [:br]
     [contract-funcs write-funcs false]]))

(defn contract-address []
  (let [v (:contract-addr @app-state)]
    [:p "contract to call:"
     [:input {:type "text"
              :value v
              :on-change #(swap! app-state assoc :contract-addr (-> % .-target .-value))}]]))

(defn connect-wallet-btn [on-click]
  (let [account (:account @w3)
        balance (:balance @w3)]
    [:div
     [:button {:on-click on-click} (if (= "" account) "Connect Wallet" account)]
     [:p "Balance: " balance]]))

(defn enable-web3! [e]
  (go
    (println "Connect Wallet  clicked")
    (if (not web3/is-metamask-installed)
      (js/alert "metamask is not installed")
      (let [[addr] (<p! (web3/request-accounts))
            balance (<p! (.getBalance (:signer @w3)))]
        (swap! w3 assoc :account addr :balance (web3/utils.formatUnits balance))))))

(defn block-number []
  [:p {:style {:color "pink"}}
      "Block eight: " @blockheight])

(defn home-page []
  [:div
   [:h2 "brownie & Reagent"]
   [block-number]
   [connect-wallet-btn enable-web3!]
   [contract-address]
   [upload-btn (:abi-file-name @app-state)]
   [result (:abi-data @app-state)]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
