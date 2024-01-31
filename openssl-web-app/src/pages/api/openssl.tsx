const { exec } = require('child_process');

export default function handler(req, res) {
  const requestType = req.body.type; // Type of OpenSSL request
  const inputData = req.body.data; // Input data for the OpenSSL command

  let command;

  switch (requestType) {
    case 'publicKey':
      command = 'openssl pkey -pubout';
      break;
    case 'createCertificate':
      // Example command for X.509 certificate creation
      command = 'openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365';
      break;
    // Add more cases as needed
    default:
      return res.status(400).json({ error: 'Invalid request type' });
  }

  const process = exec(command, (error, stdout, stderr) => {
    if (error) {
      return res.status(500).json({ error: error.message });
    }
    if (stderr) {
      return res.status(500).json({ error: stderr });
    }
    // stdout will contain the output of the OpenSSL command
    res.status(200).json({ result: stdout });
  });

  // Write input data to the stdin of the OpenSSL process, if necessary
  if (inputData) {
    process.stdin.write(inputData);
  }
  process.stdin.end();
}
