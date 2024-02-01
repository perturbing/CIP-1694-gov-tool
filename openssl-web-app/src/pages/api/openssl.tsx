import { NextApiRequest, NextApiResponse } from 'next';
const { exec } = require('child_process');

// export default function handler(req, res) {
export default function handler(req: NextApiRequest, res: NextApiResponse) {
  return new Promise<void>((resolve, reject) => {
    const requestType = req.body.type; // Type of OpenSSL request
    const inputData = req.body.data; // Input data for the OpenSSL command
    const auxData = req.body.aux; // Auxiliary data for the OpenSSL command

    let command;

    switch (requestType) {
      case 'publicKey':
        command = `echo "${inputData}" | openssl pkey -pubout`;
        break;
      case 'createCA':
        var { country, state, city, organization, organizationalUnit , commonName, validity } = auxData;
        var subj = `/C=${country}/ST=${state}/L=${city}/O=${organization}/OU=${organizationalUnit}/CN=${commonName}`;
        command = `echo "${inputData}" | openssl req -new -x509 -days ${validity} -key /dev/stdin -subj "${subj}"`;
        break;
      case 'createCSR':
        var { country, state, city, organization, organizationalUnit , commonName, validity } = auxData;
        var subj = `/C=${country}/ST=${state}/L=${city}/O=${organization}/OU=${organizationalUnit}/CN=${commonName}`;
        command = `echo "${inputData}" | openssl req -new -key /dev/stdin -subj "${subj}"`;
        break;
      case 'viewCSR':
        command = `echo "${inputData}" | openssl req -text -noout`;
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