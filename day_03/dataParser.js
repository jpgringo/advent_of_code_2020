const fs = require('fs');
const path = require('path');
const readline = require('readline');

async function getDataSet() {
  const fileStream = fs.createReadStream(path.join(__dirname, 'input_data.txt'));
  const rl = readline.createInterface({input: fileStream, crlfDelay: Infinity})
  const data = [];
  for await (const line of rl) {
    data.push(line);
  }
  return data;
}

module.exports.getDataSet = getDataSet;
