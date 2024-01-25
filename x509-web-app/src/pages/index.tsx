import React, { useState, useContext } from "react";
import { C, Lucid, M, fromHex, fromText, getAddressDetails, toHex } from "lucid-cardano"

import HomeX509 from "@/components/HomeX509";
import Create509CA from "@/components/CreateX509CA";
import IssueX509Child from "@/components/IssueX509Child";
import VerifyX509 from "@/components/VerifyX509";
import { AppStateContext } from "./_app";

import Image from "next/image"

export default function Home() {
  // State to track the active page
  const [activePage, setActivePage] = useState("home");
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
        const details = getAddressDetails(addr)
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
    if (!appState.lucid) {
      window.alert("Please connect a wallet first");
      return;
    }
    try {
      const address = await appState.lucid.wallet.address();
      const now = new Date();
      const payloadText = `Signed at: ${now.toUTCString()}`;
      const payload = fromText(payloadText);
      const signedMessage = await appState.lucid.newMessage(address, payload).sign();
      const coseKey = M.COSEKey.from_bytes(fromHex(signedMessage.key));

      // Now use the 'header' method on the M.COSEKey instance
      const publicKey = (() => {
        try {
          return C.PublicKey.from_bytes(
            coseKey.header(M.Label.new_int(
              M.Int.new_negative(
                M.BigNum.from_str("2"),
              ),
            ))?.as_bytes()!,
          );
        } catch (_e) {
          throw new Error("No public key found.");
        }
      })();

      setAppState({
        ...appState,
        pubKey: toHex(publicKey.as_bytes()),
      });
    } catch (e) {
      console.log(e)
      return;
    }
  }

  const getShortenedKey = (key:string) => {
    return `${key.substring(0, 4)}...${key.substring(key.length - 4)}`;
  };

  const copyToClipboard = () => {
    if (!appState.pubKey) {
      window.alert("Please load a public key first");
      return;
    }
    navigator.clipboard.writeText(appState.pubKey).then(() => {
      window.alert("Public key copied to clipboard!");
    }, (err) => {
      console.error('Could not copy text: ', err);
    });
  };

  // Function to render the active page component
  const renderActivePage = () => {
    switch (activePage) {
      case "home":
        return HomeX509(appState);
      case "create":
        return Create509CA(appState);
      case "issue":
        return IssueX509Child(appState);
      case "verify":
        return VerifyX509(appState);
      default:
        return HomeX509(appState);
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
                activePage === "create" && "font-bold"
              }`}
              onClick={() => setActivePage("create")}
            >
              Create CA
            </span>
            <span
              className={`cursor-pointer hover:underline ${
                activePage === "issue" && "font-bold"
              }`}
              onClick={() => setActivePage("issue")}
            >
              Issue Child
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
                className="bg-blue-500 text-white px-4 py-2 rounded flex items-center justify-center gap-2"
                onClick={connectWallet}
              >
                <Image
                  src="/lace.png" // Correct path to your image in the public folder
                  alt="Wallet Icon"
                  width={24}  // Adjust the size as needed
                  height={24} // Adjust the size as needed
                />
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