import React from 'react';
import { AppState } from "@/pages/_app";

export default function HomeX509(appState: AppState | undefined) {
  return (
    <div className="p-4">
      <h1 className="text-lg font-bold">Welcome to this X.509 Certificate Management DApp</h1>
      <p>This DApp integrates with Lace wallets on the Cardano blockchain for managing X.509 certificates. Here's what you can do:</p>
      <ul className="list-disc ml-8 mt-4">
        <li>Create Self-Signed X.509 Certificates: Generate your own CA using your public key.</li>
        <li>Issue Child Certificates: Issue child certificates from any X.509 certificate, including non-CAs.</li>
        <li>Verify X.509 Certificates: Ensure the authenticity of any X.509 certificate against a credentials tree.</li>
      </ul>
      <p className="mt-4">Our aim is to make certificate management straightforward and accessible, regardless of your familiarity with blockchain technology.</p>
    </div>
  );
}
