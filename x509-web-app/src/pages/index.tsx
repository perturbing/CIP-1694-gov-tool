import React, { useState, useContext } from "react";
import { getAddressDetails } from "lucid-cardano"

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
      lucid.selectWallet(lace);
      const addr = await lucid.wallet.address();
      const details = await getAddressDetails(addr)
      setAppState({
        ...appState,
        lucid: lucid,
        pubKeyHash: details.paymentCredential?.hash,
      });
    } catch (e) {
      console.log(e)
      return;
    }

  };

  const disconnectWallet = () => {
  setAppState(initialAppState);
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
      <header className="p-4 bg-gray-200 dark:bg-zinc-800">
        <div className="flex justify-between items-center max-w-5xl mx-auto">
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
          {appState.pubKeyHash? (
            <button
              className="bg-red-500 text-white px-4 py-2 rounded"
              onClick={disconnectWallet}
            >
              Disconnect Wallet
            </button>
          ) : (
            <button
              className="bg-blue-500 text-white px-4 py-2 rounded"
              onClick={connectWallet}
            >
              Connect Wallet
            </button>
          )}
        </div>
      </header>
      <main className="flex min-h-screen flex-col items-center justify-between p-24">
        {renderActivePage()}
      </main>
    </div>
  );
}