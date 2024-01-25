import React, { useState, useContext } from "react";
import { Lucid, getAddressDetails } from "lucid-cardano"

import Create509CA from "@/components/Create509CA";
import IssueX509Child from "@/components/IssueX509Child";
import VerifyX509 from "@/components/VerifyX509";
import { AppStateContext, initialAppState } from "./_app";

export default function Home() {
  // State to track the active page
  const [activePage, setActivePage] = useState("create");
  const { appState, setAppState } = useContext(AppStateContext);
  const { lucid } = appState;
  
  const connectWallet = async () => {
    try {
      if (!window.cardano.lace) {
        window.alert("Please install a Lace Wallet");
        return;
      }
      const lace = await window.cardano.lace.enable();
      if (lucid) {
        lucid.selectWallet(lace);
        const addr = await lucid.wallet.address();
        const details = await getAddressDetails(addr)
        setAppState({
          ...appState,
          lucid: lucid,
          pubKeyHash: details.paymentCredential?.hash,
        });
      }
    } catch (e) {
      console.log(e)
      return;
    }
  };

  const disconnectWallet = async () => {
    const newLucid = await Lucid.new();
    setAppState({
      ...appState,
      lucid: newLucid,
      pubKeyHash: undefined,
      pubKey: undefined,
    });
  };

  const loadPublicKey = async () => {
    console.log("Loading public key");
    setAppState({
      ...appState,
      pubKey: "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
    });
  }

  const getShortenedKey = (key) => {
    return `${key.substring(0, 4)}...${key.substring(key.length - 4)}`;
  };

  const copyToClipboard = () => {
    navigator.clipboard.writeText(appState.pubKey).then(() => {
      window.alert("Public key copied to clipboard!");
    }, (err) => {
      console.error('Could not copy text: ', err);
    });
  };

  // Function to render the active page component
  const renderActivePage = () => {
    switch (activePage) {
      case "create":
        return Create509CA();
      case "issue":
        return IssueX509Child();
      case "verify":
        return VerifyX509();
      default:
        return Create509CA();
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
                activePage === "create" && "font-bold"
              }`}
              onClick={() => setActivePage("create")}
            >
              Create X.509 CA
            </span>
            <span
              className={`cursor-pointer hover:underline ${
                activePage === "issue" && "font-bold"
              }`}
              onClick={() => setActivePage("issue")}
            >
              Issue X.509 Child
            </span>
            <span
              className={`cursor-pointer hover:underline ${
                activePage === "verify" && "font-bold"
              }`}
              onClick={() => setActivePage("verify")}
            >
              Verify X.509
            </span>
          </div>

          {/* Right side - Wallet connection and public key info */}
          <div className="flex items-center gap-4">
            {appState.pubKeyHash ? (
              <>
                <button
                  className="bg-red-500 text-white px-4 py-2 rounded"
                  onClick={disconnectWallet}
                >
                  Disconnect Wallet
                </button>
                {appState.pubKey ? (
                  <div className="flex items-center gap-2">
                    <span>PubKey: {getShortenedKey(appState.pubKey)}</span>
                    <button 
                    className="bg-blue-500 text-white px-2 py-1 rounded"
                    onClick={copyToClipboard}
                    >
                      Copy
                    </button>
                  </div> 
                ) : (
                  <button
                    className="bg-green-500 text-white px-4 py-2 rounded"
                    onClick={loadPublicKey}
                  >
                    Load Public Key
                  </button>
                )}
              </>
            ) : (
              <button
                className="bg-blue-500 text-white px-4 py-2 rounded"
                onClick={connectWallet}
              >
                Connect Wallet
              </button>
            )}
          </div>
        </div>
      </header>

      {/* Main content */}
      <main className="flex min-h-screen flex-col items-center justify-between p-24">
        {renderActivePage()}
      </main>

    </div>
  );
}