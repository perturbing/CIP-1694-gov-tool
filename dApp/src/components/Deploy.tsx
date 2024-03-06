import React, { useContext } from 'react';
import { AppStateContext } from '@/pages/_app';
import { C, fromHex, fromText, sha256, toHex } from 'lucid-cardano';
import { callCardanoCli, getShortenedKey } from '@/utils/utils';

const InputBox = ({ value, onChange, placeholder, multiline }) => {
    return multiline ? (
      <textarea
        value={value}
        onChange={onChange}
        placeholder={placeholder}
        className="mt-2 mb-2 p-2 border rounded w-full bg-white text-gray-700 h-32 resize-none" // Set a default height and disable resizing
      />
    ) : (
      <input
        type="text"
        value={value}
        onChange={onChange}
        placeholder={placeholder}
        className="mt-2 mb-2 p-2 border rounded w-full bg-white text-gray-700"
      />
    );
};

function createDataStructure(owners, users) {
  // Helper function to convert an individual owner/user entry to the required field structure
  const convertEntry = (entry) => ({
    constructor: 0,
    fields: [
      { bytes: entry.pubkeyHash },
      { bytes: entry.certificateHash }
    ]
  });

  // Convert owners and users lists
  const ownersList = owners.map(convertEntry);
  const usersList = users.map(convertEntry);

  // Construct the final data structure
  const plutusDatum = {
    constructor: 0,
    fields: [
      { list: ownersList },
      { list: usersList }
    ]
  };

  return plutusDatum;
}
  
  
export default function Deploy({
  newOwner,
  newUser,
  handleAddOwner,
  handleAddUser,
  deployedState,
  owners,
  users,
  addOwner,
  updateOwner,
  addUser,
  updateUser,
  removeOwner,
  removeUser,
  orchestratorAddress,
  ccNftPolicyId,
  alwaysTrueScript,
  lockAddress,
  privKey 
}) {

  const deployDapp = async () => {
      try {
        // step 1: convert the list to a datum
        const plutusDatum = JSON.stringify(createDataStructure(owners, users));
        callCardanoCli('query-address', orchestratorAddress,"Error retrieving utxo's at orchestrator address").then((res) => {
          const utxoIn = JSON.parse(res.result)[0]
          // step 2: create a transaction with the datum
          callCardanoCli('build-deploy-tx', {lockAddress,orchestratorAddress,ccNftPolicyId,plutusDatum,alwaysTrueScript,utxoIn},"error building deploy tx").then((res2) => {
            const tx = res2.result
            // step 3: sign the transaction with the orchestrator's private key
            callCardanoCli('sign-tx', {tx,privKey},"error signing tx").then((res3) => {
              // console.log(res3.result)
              const signedTx = res3.result
              // step 4: submit the transaction to the blockchain
              callCardanoCli('submit-tx', signedTx,"error submitting tx").then((res4) => {
                // console.log("dApp deployed successfully.");
                alert("Transaction submitted and dApp successfully deployed.");
              }).catch(error => {
                console.error("Failed to submit tx:", error);
              });
            }).catch(error => {
              console.error("Failed to sign tx:", error);
            });
          }).catch(error => {
            console.error("Failed to build deploy tx:", error);
          });
        }).catch(error => {
          console.error("Failed to query address:", error);
        });
      } catch (error) {
        console.error("Failed to deploy dApp:", error);
      }
    };

  const displayOwnerUser = (list, removeFunction) => (
    <ul>
      {list.map((item, index) => (
        <li key={index} className="flex justify-between items-center mt-1">
          <span className="text-gray-700">{item.pubkeyHash ? "pkh: "+ getShortenedKey(item.pubkeyHash) : 'Invalid entry'}</span>
          <button onClick={() => removeFunction(index)} className="ml-2 bg-red-500 hover:bg-red-700 text-white font-bold py-1 px-2 rounded text-xs">
          Remove
          </button>
        </li>
      ))}
    </ul>
  );
  
  if (deployedState) {
    return <div className="p-4 bg-gray-100 rounded shadow text-gray-600 ">dApp already deployed.</div>;
  }
  
  return (
    <div className="p-4 bg-gray-100 rounded shadow flex flex-wrap justify-between">
      {/* Owners Column */}
      <div className="w-full md:w-1/2">
        <p className="text-center text-lg text-gray-600">Owners</p>
        <InputBox
          value={newOwner.certificate}
          onChange={(e) => {handleAddOwner('certificate', e.target.value)}}
          placeholder="Paste certificate string"
          multiline={true}
        />
        <InputBox
          value={newOwner.pubkey}
          onChange={(e) => {handleAddOwner('pubkey', e.target.value)}}
          placeholder="Paste public key"
          multiline={false}
        />
        <button
          onClick={(e) => {
            addOwner();
            updateOwner(owners.length, {
              certificate: newOwner.certificate, 
              certificateHash: toHex(sha256(fromHex(fromText(newOwner.certificate)))),
              pubkey: newOwner.pubkey, 
              pubkeyHash: toHex(C.hash_blake2b224(fromHex(newOwner.pubkey)))
            });
          }}
          className="mt-2 bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
        >
        +
        </button>
          {displayOwnerUser(owners, removeOwner)}
        </div>
    
        {/* Users Column */}
        <div className="w-full md:w-1/2">
          <p className="text-center text-lg text-gray-600">Users</p>
          <InputBox
            value={newUser.certificate}
            onChange={(e) => {handleAddUser('certificate', e.target.value)}}
            placeholder="Paste certificate string"
            multiline={true}
          />
          <InputBox
            value={newUser.pubkey}
            onChange={(e) => {handleAddUser('pubkey', e.target.value)}}
            placeholder="Paste public key"
            multiline={false}
          />
          <button
            onClick={(e) => {
              addUser();
              updateUser(users.length, {                                
                certificate: newUser.certificate, 
                certificateHash: toHex(sha256(fromHex(fromText(newUser.certificate)))),
                pubkey: newUser.pubkey, 
                pubkeyHash: toHex(C.hash_blake2b224(fromHex(newUser.pubkey)))
              });
            }}
          className="mt-2 bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
          >
          +
          </button>
            {displayOwnerUser(users, removeUser)}
          </div>

          {/* Deploy dApp Button */}
          <div className="w-full text-center mt-4">
            <button
            onClick={deployDapp}
            className="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded"
            >
            Deploy dApp
            </button>
          </div>
        </div>
)};