(ns demo.web3
  (:require ["ethers" :as ethers]
            [goog.object]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs.core.async :refer [go]]))

(def ethereum (.-ethereum js/window))
(def utils (.-utils ethers))

(def is-metamask-installed
  (if (and ethereum (.-isMetaMask ethereum))
    true
    false))

(defn to-readable-abi [json-abi]
  (-> json-abi
      (utils.Interface.)
      (.format ethers/FormatTypes.-full)))

(defn request-accounts []
  (.request ethereum #js {"method" "eth_requestAccounts"}))

(defn get-provider []
  (new (.-Web3Provider (.-providers ethers)) ethereum))

(defn get-contract [addr abi provider]
  (ethers/Contract. addr (clj->js abi) provider))

(defn call [contract method args]
  (do (js/console.log "call: " method args)
        (apply (aget contract method) (clj->js args))))
