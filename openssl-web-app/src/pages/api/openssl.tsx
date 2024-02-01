import { NextApiRequest, NextApiResponse } from 'next';
import { exec } from 'child_process';

// export default function handler(req, res) {
export default function handler(req: NextApiRequest, res: NextApiResponse) {
  return new Promise<void>(async (resolve, reject) => {
    const requestType = req.body.type; // Type of OpenSSL request
    const inputData = req.body.data; // Input data for the OpenSSL command

    let command: string;
    let subj: string;
    
    // Common logic for 'createCA' and 'createCSR'
    if (requestType === 'createCA' || requestType === 'createCSR') {
        const { privKey, formData } = inputData;
        const { country, state, city, organization, organizationalUnit, commonName } = formData;
        subj = `/C=${country}/ST=${state}/L=${city}/O=${organization}/OU=${organizationalUnit}/CN=${commonName}`;
    }
    
    switch (requestType) {
      case 'publicKey':
          command = `openssl pkey -in <(echo "${inputData}") -pubout`;
          break;
      case 'createCA':
          command = `openssl req -new -x509 -days ${inputData.formData.validity} -key <(echo "${inputData.privKey}") -subj "${subj}"`;
          break;
      case 'createCSR':
          command = `echo "${inputData.privKey}" | openssl req -new -key /dev/stdin -subj "${subj}"`;
          break;
      case 'viewCSR':
        command = `openssl req -in <(echo "${inputData}") -text -noout`;
        break;
      case 'viewX509':
        command = `openssl x509 -in <(echo "${inputData}") -text -noout`;
        break;
      default:
        return res.status(400).json({ error: 'Invalid request type' });
    }

    // Execute the command
    const process = exec(command, (error, stdout, stderr) => {
      if (error) {
        res.status(500).json({ error: error.message });
        return resolve(); // Resolve the promise after sending response
      }
      if (stderr) {
        res.status(500).json({ error: stderr });
        return resolve(); // Resolve the promise after sending response
      }
      res.status(200).json({ result: stdout });
      console.log(stdout);
      console.log(stderr);
      console.log(error);
      resolve(); // Resolve the promise after sending response
    });

  });
}