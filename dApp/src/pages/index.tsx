import React, { useState, useContext } from "react"
import { C, Lucid, M, fromHex, fromText, getAddressDetails, toHex } from "lucid-cardano"

import Landing from "@/components/Landing"
import Verify from "@/components/Verify"
import Deploy from "@/components/Deploy"
import { AppStateContext } from "./_app"

import { processPemKey, callOpenSSL, getShortenedKey, copyToClipboard } from "@/utils/utils"

export default function Home() {
  // State to track the active page
  const [activePage, setActivePage] = useState("home")
  const { appState, setAppState } = useContext(AppStateContext)

  const openOpenSSLKey = async () => {
    const fileInput = document.createElement('input')
    fileInput.type = 'file'
    fileInput.style.display = 'none'
  
    fileInput.onchange = async (e) => {
      const file = e.target.files[0]
      if (file) {
        const reader = new FileReader()
        reader.onload = async (e) => {
          const privOpenSSL = e.target.result
          
          // call api to calculate public key from private key, upon succes store both values.
          callOpenSSL("publicKey",privOpenSSL as string, "that was not a valid ed25519 openSSL private key")
            .then(data => {
              const pk = processPemKey(data.result,"302a300506032b6570032100")
              const privRaw = processPemKey(privOpenSSL as string,"302e020100300506032b657004220420")
              const privKey = C.PrivateKey.from_normal_bytes(fromHex(privRaw)).to_bech32()
              // load private key into lucid for signing.
              appState.lucid?.selectWalletFromPrivateKey(privKey)
              setAppState({
                ...appState,
                openSSLPrivKey: privOpenSSL as string,
                openSSLPubKey: data.result,
                privateKey: privKey,
                publicKey: pk,
                publicKeyHash: toHex(C.hash_blake2b224(fromHex(pk)))
              })
            })
            .catch(error => {
              window.alert("Error calling API: " + error)
            })
        }
        reader.readAsText(file)
      }
    }
    fileInput.click();
  }

  const disconnectOpenSSLKey = async () => {
    const newLucid = await Lucid.new()
    console.log()
    setAppState({
      ...appState,
      openSSLPrivKey: undefined,
      openSSLPubKey: undefined,
      publicKey: undefined,
      publicKeyHash: undefined,
    });
  };

  // Function to render the active page component
  const renderActivePage = () => {
    switch (activePage) {
      case "home":
        return <Landing />;
      case "deploy":
        return <Deploy />;
      case "verify":
        return <Verify />;
      default:
        return <Home />;
    }
  };

  return (
    <div>

      {/* Header */}
      <header className="p-4 bg-gray-200 dark:bg-zinc-800">
        <div className="flex justify-between items-center max-w-5xl mx-auto">

          {/* Left side - Navigation links */}
          <div className="flex gap-4">
            <span 
              className={`cursor-pointer hover:underline ${
                activePage === "home" && "font-bold"
              }`}
              onClick={() => setActivePage("home")}
            >
              Home
            </span>
            <span 
            className={`cursor-pointer hover:underline ${
              activePage === "Deploy" && "font-bold"
            }`}
            onClick={() => setActivePage("deploy")}
            >
            Deploy
            </span>
            <span
              className={`cursor-pointer hover:underline ${
                activePage === "verify" && "font-bold"
              }`}
              onClick={() => setActivePage("verify")}
            >
              Verify
            </span>
          </div>

          {/* Right side - Wallet connection and public key info */}
          <div className="flex items-center gap-4">
            {appState.openSSLPrivKey ? (
              <>
                <button
                  className="bg-red-500 text-white px-4 py-2 rounded"
                  onClick={disconnectOpenSSLKey}
                >
                  Disconnect OpenSSL key
                </button>
              </>
            ) : (
              <button
                className="bg-blue-500 text-white px-4 py-2 rounded flex items-center justify-center gap-2"
                onClick={openOpenSSLKey}
              >
                Connect OpenSSL key 
              </button>
            )}
          </div>
        </div>
      </header>

      {/* Main content */}
      <main className="flex min-h-screen flex-col items-center justify-between p-24">
              {!appState.openSSLPrivKey ? (
                <div className="p-4 bg-gray-100 rounded shadow">
                  <p className="text-center text-lg text-gray-600">Please open your OpenSSL private key pem file via the blue button in the top-right corner.</p>
                  <p className="text-center text-lg text-gray-600">Note that this application does server-side computation with this private key!</p>
                  <p className="text-center text-lg text-gray-600">Thus, make sure that you host this application yourself to avoid leaking keys.</p>
                </div>
              ) : (
                renderActivePage()
              )}
              {appState.publicKey ? (
                <div className="flex items-center gap-2">
                    <span>Public Key: {getShortenedKey(appState.publicKey)}</span>
                    <button 
                    className="bg-blue-500 text-white px-2 py-1 rounded"
                    onClick={() => copyToClipboard(appState.publicKey || "", "Public key copied to clipboard")}
                    >
                      Copy public key
                    </button>
                    <span>Public Key Hash: {getShortenedKey(appState.publicKeyHash || "")}</span>
                    <button 
                    className="bg-blue-500 text-white px-2 py-1 rounded"
                    onClick={() => copyToClipboard(appState.publicKeyHash || "", "Public key hash copied to clipboard")}
                    >
                      Copy public key hash
                    </button>
                </div> 
              ) : (
                <div>
                </div>
              )}
        </main>
    </div>
  );
}