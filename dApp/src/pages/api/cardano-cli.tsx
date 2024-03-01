import { NextApiRequest, NextApiResponse } from 'next';
import { execSync } from 'child_process';

// export default function handler(req, res) {
export default function handlerCardanoCli(req: NextApiRequest, res: NextApiResponse) {
  return new Promise<void>(async (resolve, reject) => {
    const requestType = req.body.type; // Type of cardano-cli request
    const inputData = req.body.data; // Input data for the cardano-cli command

    let command: string;
    let subj: string;
    const magicNr = 42;
    // console.log("api: " + inputData);
    
    switch (requestType) {
      case 'version':
          command = `cardano-cli --version`;
          break;
      case 'script-address':
          command = `cardano-cli address build --testnet-magic ${magicNr} --payment-script-file <(echo '${inputData}')`;
          break;
      case 'policy-id':
          command = `cardano-cli transaction policyid --script-file <(echo '${inputData}')`;
          break;
      case 'query-address':
          command = `cardano-cli query utxo --testnet-magic ${magicNr} --address ${inputData} --out-file /dev/stdout | jq -r 'keys' `;
          break;
      default:
        return res.status(400).json({ error: 'Invalid request type' });
    }

    try {
      const output = execSync(command).toString();
      res.status(200).json({ result: output });
    } catch (error) {
      if (error.stdout) {
        res.status(500).json({ error: error.stdout}); 
        return resolve();
      }
      if (error.stderr) {
        res.status(500).json({ error: error.stderr});
        return resolve();
      }
    }
  
  });
}