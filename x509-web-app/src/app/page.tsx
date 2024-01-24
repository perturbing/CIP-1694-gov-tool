"use client";

import React, { useState, useEffect } from "react";
import Image from "next/image";
import Link from "next/link";
// import { fromText } from "translucent-cardano"

import Create509CA from "./Create509CA";
import Issue509Child from "./IssueX509Child";
import Verify509 from "./VerifyX509";

// Import or define your page components here
const CreateX509CA = () => Create509CA();
const IssueX509Child = () => Issue509Child();
const VerifyX509 = () => Verify509();

export default function Home() {
  // State to track the active page
  const [activePage, setActivePage] = useState("create");
  
  // const payload = fromText("Hello from translucent!");
  
  // Function to render the active page component
  const renderActivePage = () => {
    switch (activePage) {
      case "create":
        return <CreateX509CA />;
      case "issue":
        return <IssueX509Child />;
      case "verify":
        return <VerifyX509 />;
      default:
        return <CreateX509CA />;
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
          <button className="bg-blue-500 text-white px-4 py-2 rounded">
            Connect Wallet
          </button>
        </div>
      </header>
      <main className="flex min-h-screen flex-col items-center justify-between p-24">
        {renderActivePage()}
      </main>
    </div>
  );
}