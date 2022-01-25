# Reagent & web3(etherjs) demo

A simple local `etherscan` like contract interact demo using clojurescript + reagent + etherjs.

Try a IFPS hosted [demo](https://gateway.pinata.cloud/ipfs/QmeAwBwdWYGa1vqerqcitRmA59We6uqCKMN2nEAN4R3D3P/?preview=1)

Glitch hosted [demo](https://reagent-web3-demo.glitch.me/)

## usage

1. connect wallet
2. input your contract address
3. upload abi json file
4. now you can read/write contract

### Development mode
```
npm install
npx shadow-cljs watch app
```
start a ClojureScript REPL
```
npx shadow-cljs browser-repl
```
### Building for production

```
npx shadow-cljs release app
```
