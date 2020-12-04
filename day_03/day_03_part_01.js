const fs = require('fs');
const path = require('path');
const readline = require('readline');

async function getDataSet() {
  const fileStream = fs.createReadStream(path.join(__dirname, 'input_data.txt'));
  const rl = readline.createInterface({input:fileStream, crlfDelay: Infinity})
  const data = [];
  for await (const line of rl) {
    data.push(line);
  }
  return data;
}

(function main() {
  getDataSet().then(inputData => {
    const tileWidth = inputData[0].length; // just assuming all input lines are the same length
    const treeCount = inputData.reduce((acc, row, i) => {
      const xPos = (i * 3) % tileWidth;
      return row[xPos] === '#' ? acc + 1 : acc;
    }, 0);
    console.log(`treeCount:`, treeCount);
  });
})();
